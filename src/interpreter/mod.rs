mod values;

use std::borrow::Cow;

use values::Value;
mod scope;
use scope::Scope;
mod builtins;

use crate::{
    extract_type_value,
    interpreter::{
        scope::ValueReference,
        values::{ArrayValue, BuiltinIterator, ExclusiveRangeValue, InclusiveRangeValue},
    },
    parser::{
        BlockExpression, Expression, FunctionCallExpression, IfExpression, LogicalComparison,
        LogicalComparisonExpression, MathOperation, MathOperationExpression, OrderComparison,
        OrderComparisonExpression, Pattern, Statement, StatementBody, AST,
    },
    primitives_pattern,
    shared::{name_bihasher::NameHash, no_hash_map::NoHashMap, types::Type},
};

use miette::{bail, Result};

use self::{
    scope::{LoopAction, ScopeKind, Variable},
    values::{BuiltinFunctionValue, FunctionValue, SliceValue, TupleValue},
};

macro_rules! handle_loop_ending {
    ($self:ident, $label:tt) => {
        match $self.current_scope().kind {
            ScopeKind::Loop(Some(LoopAction::Continue)) => {
                $self.pop_scope();
                continue $label;
            }
            ScopeKind::Loop(Some(LoopAction::Break)) => {
                $self.pop_scope();
                break $label;
            },
            _ => $self.pop_scope(),
        }
    };
}

pub struct Interpreter<'a> {
    ast: AST<'a>,

    current_scope: usize,
    scopes: Vec<Scope<'a>>,

    builtins: NoHashMap<Value<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new(ast: AST<'a>) -> Self {
        Self {
            ast,

            current_scope: 0,
            scopes: vec![Scope::with_builtins(ScopeKind::Block)],

            builtins: NoHashMap::default(),
        }
    }

    pub fn current_scope(&self) -> &Scope<'a> {
        &self.scopes[self.current_scope]
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope<'a> {
        &mut self.scopes[self.current_scope]
    }

    pub fn new_scope(&mut self, kind: ScopeKind<'a>) {
        self.scopes.push(Scope::new(kind));
        self.current_scope += 1;
    }

    pub fn pop_scope(&mut self) -> Scope<'a> {
        let scope = self.scopes.remove(self.current_scope);
        self.current_scope -= 1;
        scope
    }

    pub fn closest_scope_of_kind_mut(&mut self, kind: ScopeKind<'a>) -> Option<&mut Scope<'a>> {
        self.scopes
            .iter_mut()
            .rev()
            .find(|scope| std::mem::discriminant(&scope.kind) == std::mem::discriminant(&kind))
    }

    pub fn get_variable(&mut self, name: &NameHash) -> Option<&mut Variable<'a>> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.variables.get_mut(name))
    }

    pub fn get_builtin(&self, name: &NameHash) -> Option<&Value<'a>> {
        self.builtins.get(name)
    }

    pub fn evaluate(mut self) -> Result<()> {
        while let Some(statement) = self.ast.body.pop_front() {
            self.evaluate_statement(&statement)?;
        }
        Ok(())
    }

    pub fn evaluate_statement(&mut self, statement: &Statement<'a>) -> Result<()> {
        match statement {
            Statement::Expression(expression) => {
                self.evaluate_expression(expression)?;
            }
            Statement::VariableDeclaration(name, mutable, value_type, value) => {
                self.evaluate_variable_declaration(name, mutable, value_type, value)?;
            }
            Statement::Function(name, parameters, return_type, body) => {
                let parameters = &*parameters.as_ref();
                let body = &*body.as_ref();

                let function = Value::Function(FunctionValue(
                    parameters.into(),
                    // This clone only happens once, so it's not that bad
                    return_type.clone(),
                    body.into(),
                ));

                self.current_scope_mut()
                    .set_immutable_variable(*name, function);
            }
            Statement::WhileLoop(condition, body) => self.evaluate_while_loop(condition, body)?,
            Statement::Loop(body) => self.evaluate_loop(body)?,
            Statement::ForLoop(pattern, expression, body) => {
                let iterable = &self.evaluate_expression(expression)?;
                self.evaluate_for_loop(iterable, pattern, body)?
            }

            Statement::Break => {
                if let Some(scope) = self.closest_scope_of_kind_mut(ScopeKind::Loop(None)) {
                    scope.kind = ScopeKind::Loop(Some(LoopAction::Break));
                } else {
                    panic!("You can only continue loops, got {statement:?}");
                }
            }

            Statement::Continue => {
                if let Some(scope) = self.closest_scope_of_kind_mut(ScopeKind::Loop(None)) {
                    scope.kind = ScopeKind::Loop(Some(LoopAction::Continue));
                } else {
                    panic!("You can only continue loops, got {statement:?}");
                }
            }

            Statement::Return(value) => {
                let value = self.evaluate_expression(value)?;

                if let Some(scope) = self.closest_scope_of_kind_mut(ScopeKind::Function(None)) {
                    scope.kind = ScopeKind::Function(Some(value));
                } else {
                    panic!("You can only return from functions, got {value:?}");
                }
            }

            statement => todo!("TODO STATEMENT {statement:?}"),
        }

        Ok(())
    }

    pub fn evaluate_expression(&mut self, expression: &Expression<'a>) -> Result<Value<'a>> {
        Ok(match expression {
            // values
            Expression::Integer32(value) => Value::Integer32(*value),
            Expression::Integer64(value) => Value::Integer64(*value),
            Expression::Float32(value) => Value::Float32(*value),
            Expression::Float64(value) => Value::Float64(*value),
            Expression::Boolean(value) => Value::Boolean(*value),
            Expression::String(value) => Value::String(value.to_string()),
            Expression::Array(value) => {
                let size = value.len();
                let items = value
                    .iter()
                    .map(|expression| match self.evaluate_expression(expression) {
                        Ok(value) => Ok(Cow::Owned(value)),
                        Err(err) => Err(err),
                    })
                    .collect::<Result<Box<[_]>>>()?;
                Value::Array(values::ArrayValue(items, size))
            }
            Expression::Slice(value) => {
                let items = value
                    .iter()
                    .map(|expression| match self.evaluate_expression(expression) {
                        Ok(value) => Ok(Cow::Owned(value)),
                        Err(err) => Err(err),
                    })
                    .collect::<Result<Vec<_>>>()?;
                Value::Slice(SliceValue(items))
            }
            Expression::Tuple(value) => {
                let items = value
                    .iter()
                    .map(|expression| match self.evaluate_expression(expression) {
                        Ok(value) => Ok(Cow::Owned(value)),
                        Err(err) => Err(err),
                    })
                    .collect::<Result<Box<[_]>>>()?;
                Value::Tuple(TupleValue(items))
            }

            Expression::ExclusiveRange(from, to) => {
                let from = self.evaluate_expression(from)?;
                let to = self.evaluate_expression(to)?;
                self.evaluate_exclusive_range(from.as_direct_self(), to.as_direct_self())?
            }
            Expression::InclusiveRange(from, to) => {
                let from = self.evaluate_expression(from)?;
                let to = self.evaluate_expression(to)?;
                self.evaluate_inclusive_range(from.as_direct_self(), to.as_direct_self())?
            }

            Expression::MathOperation(math_operation) => {
                self.evaluate_math_operation_expression(math_operation)?
            }
            Expression::LogicalComparison(logical_comparison) => {
                self.evaluate_logical_comparison_expression(logical_comparison)?
            }
            Expression::OrderComparison(order_comparison) => {
                self.evaluate_order_comparison_expression(order_comparison)?
            }
            Expression::Negate(expression) => {
                let value = self.evaluate_expression(expression)?;
                let value = value.as_direct_ref();
                extract_type_value!(value, Boolean);
                Value::Boolean(!value)
            }
            Expression::ContainedExpression(expression) => self.evaluate_expression(expression)?,

            Expression::If(if_expression) => self.evaluate_if_expression(if_expression)?,
            Expression::Block(block_expression) => {
                self.evaluate_block_expression(block_expression)?
            }

            Expression::VariableReference(name) => {
                let variable = self.get_variable(name);
                match variable {
                    Some(variable) => variable.as_value_reference(),
                    None => match self.get_builtin(name) {
                        Some(value) => value.clone(),
                        None => {
                            let unhashed_name = self.ast.bihasher.unhash(name);
                            println!("{:?}", self.ast.bihasher);
                            bail!("Variable {unhashed_name:?} ({name:?}) not found <vr>")
                        }
                    },
                }
            }
            Expression::VariableAssignment(name, value) => {
                let value = self.evaluate_expression(value)?;
                self.evaluate_assignment(value.as_direct_ref(), name)?
            }

            Expression::AccessProperty(value, property) => {
                let value = self.evaluate_expression(value)?;

                match value {
                    _ => todo!("access properties"),
                }
            }

            Expression::FunctionCall(function_call_expression) => {
                self.evaluate_function_call_expression(function_call_expression)?
            }

            expression => todo!("TODO EXPR: {expression:?}"),
        })
    }

    fn evaluate_statement_body(&mut self, body: &StatementBody<'a>) -> Result<()> {
        for statement in body.iter() {
            self.evaluate_statement(statement)?;

            match &self.current_scope().kind {
                ScopeKind::Loop(Some(_)) => break,
                ScopeKind::Function(Some(_)) => break,
                _ => {}
            }
        }

        Ok(())
    }

    fn evaluate_variable_declaration(
        &mut self,
        pattern: &Pattern,
        mutable: &bool,
        value_type: &Type,
        value: &Expression<'a>,
    ) -> Result<Value<'a>> {
        let value = self.evaluate_expression(value)?;

        if !matches!(value_type, Type::Unknown) {
            value.assert_type_of(value_type)?;
        }

        self.evaluate_value_into_pattern(Cow::Owned(value), pattern, &mut Some(*mutable))?;
        Ok(Value::None)
    }

    fn evaluate_math_operation(
        &mut self,
        left: &Value<'a>,
        right: &Value<'a>,
        operation: &MathOperation,
    ) -> Result<Value<'a>> {
        left.assert_type_of(&right.into())?;

        Ok(match operation {
            MathOperation::Add => match (left, right) {
                (Value::Integer32(left), Value::Integer32(right)) => {
                    let (value, overflow) = left.overflowing_add(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer32(value)
                }
                (Value::Integer64(left), Value::Integer64(right)) => {
                    let (value, overflow) = left.overflowing_add(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer64(value)
                }
                (Value::Float32(left), Value::Float32(right)) => Value::Float32(left + right),
                (Value::Float64(left), Value::Float64(right)) => Value::Float64(left + right),
                (Value::String(left), Value::String(right)) => {
                    Value::String(format!("{}{}", left, right))
                }
                _ => panic!("You can't add types other than integers, floats or strings, got {left:?} and {right:?}"),
            },
            MathOperation::Subtract => match (left, right) {
                (Value::Integer32(left), Value::Integer32(right)) => {
                    let (value, overflow) = left.overflowing_sub(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer32(value)
                }
                (Value::Integer64(left), Value::Integer64(right)) => {
                    let (value, overflow) = left.overflowing_sub(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer64(value)
                }
                (Value::Float32(left), Value::Float32(right)) => Value::Float32(left - right),
                (Value::Float64(left), Value::Float64(right)) => Value::Float64(left - right),
                _ => panic!("You can't subtract types other than integers or floats, got {left:?} and {right:?}"),
            },
            MathOperation::Multiply => match (left, right) {
                (Value::Integer32(left), Value::Integer32(right)) => {
                    let (value, overflow) = left.overflowing_mul(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer32(value)
                }
                (Value::Integer64(left), Value::Integer64(right)) => {
                    let (value, overflow) = left.overflowing_mul(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer64(value)
                }
                (Value::Float32(left), Value::Float32(right)) => Value::Float32(left * right),
                (Value::Float64(left), Value::Float64(right)) => Value::Float64(left * right),
                _ => panic!("You can't multiply types other than integers or floats, got {left:?} and {right:?}"),
            },
            MathOperation::Divide => match (left, right) {
                (Value::Integer32(left), Value::Integer32(right)) => {
                    let (value, overflow) = left.overflowing_div(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer32(value)
                }
                (Value::Integer64(left), Value::Integer64(right)) => {
                    let (value, overflow) = left.overflowing_div(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer64(value)
                }
                (Value::Float32(left), Value::Float32(right)) => Value::Float32(left / right),
                (Value::Float64(left), Value::Float64(right)) => Value::Float64(left / right),
                _ => panic!("You can't divide types other than integers or floats, got {left:?} and {right:?}"),
            },
            MathOperation::Modulo => match (left, right) {
                (Value::Integer32(left), Value::Integer32(right)) => {
                    let (value, overflow) = left.overflowing_rem(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer32(value)
                }
                (Value::Integer64(left), Value::Integer64(right)) => {
                    let (value, overflow) = left.overflowing_rem(*right);
                    if overflow {
                        bail!("Integer overflow");
                    }
                    Value::Integer64(value)
                }
                (Value::Float32(left), Value::Float32(right)) => Value::Float32(left % right),
                (Value::Float64(left), Value::Float64(right)) => Value::Float64(left % right),
                _ => panic!("You can't modulo types other than integers or floats, got {left:?} and {right:?}"),
            },
        })
    }

    fn evaluate_math_operation_expression(
        &mut self,
        expression: &MathOperationExpression<'a>,
    ) -> Result<Value<'a>> {
        let MathOperationExpression(left, right, operation) = expression;
        let left = &self.evaluate_expression(left)?;
        let right = &self.evaluate_expression(right)?;
        self.evaluate_math_operation(left.as_direct_ref(), right.as_direct_ref(), operation)
    }

    fn evaluate_logical_comparison_expression(
        &mut self,
        expression: &LogicalComparisonExpression<'a>,
    ) -> Result<Value<'a>> {
        let LogicalComparisonExpression(left, right, comparison) = expression;
        let left = &self.evaluate_expression(left)?;
        let right = &self.evaluate_expression(right)?;
        self.evaluate_logical_comparison(left.as_direct_ref(), right.as_direct_ref(), comparison)
    }

    fn evaluate_logical_comparison(
        &mut self,
        left: &Value<'a>,
        right: &Value<'a>,
        comparison: &LogicalComparison,
    ) -> Result<Value<'a>> {
        let (Value::Boolean(left), Value::Boolean(right)) = (left, right) else {
            panic!("You can only logically compare booleans, (got {left:?} and {right:?})")
        };

        Ok(match comparison {
            // logical comparison
            LogicalComparison::And => Value::Boolean(*left && *right),
            LogicalComparison::Or => Value::Boolean(*left || *right),
            LogicalComparison::Nor => Value::Boolean(!(*left || *right)),
            LogicalComparison::Nand => Value::Boolean(!(*left && *right)),
        })
    }

    fn evaluate_order_comparison_expression(
        &mut self,
        expression: &OrderComparisonExpression<'a>,
    ) -> Result<Value<'a>> {
        let OrderComparisonExpression(left, right, comparison) = expression;
        let left = &self.evaluate_expression(left)?;
        let right = &self.evaluate_expression(right)?;
        self.evaluate_order_comparison(left.as_direct_ref(), right.as_direct_ref(), comparison)
    }

    fn evaluate_order_comparison(
        &mut self,
        left: &Value<'a>,
        right: &Value<'a>,
        comparison: &OrderComparison,
    ) -> Result<Value<'a>> {
        if !left.is_type_of(&right.into()) {
            panic!("You can only compare values of the same type: {left:?} != {right:?}");
        }

        Ok(match comparison {
                    OrderComparison::Equal => match (left, right) {
                        (Value::Integer32(left), Value::Integer32(right)) => {
                            Value::Boolean(left == right)
                        }
                        (Value::Integer64(left), Value::Integer64(right)) => {
                            Value::Boolean(left == right)
                        }
                        (Value::Float32(left), Value::Float32(right)) => {
                            Value::Boolean(left == right)
                        }
                        (Value::Float64(left), Value::Float64(right)) => {
                            Value::Boolean(left == right)
                        }
                        (Value::Boolean(left), Value::Boolean(right)) => {
                            Value::Boolean(left == right)
                        }
                        (Value::String(left), Value::String(right)) => {
                            Value::Boolean(left == right)
                        }
                        (Value::Array(ArrayValue(left, _)), Value::Array(ArrayValue(right, _))) => {
                            Value::Boolean(left == right)
                        }
                        (Value::Slice(left), Value::Slice(right)) => {
                            Value::Boolean(left == right)
                        }
                        (Value::Tuple(left), Value::Tuple(right)) => {
                            Value::Boolean(left == right)
                        }
                        _ => panic!("You can't compare types for equality other than integers, floats, booleans or strings"),
                    },
                    OrderComparison::NotEqual => match (left, right) {
                        (Value::Integer32(left), Value::Integer32(right)) => {
                            Value::Boolean(left != right)
                        }
                        (Value::Integer64(left), Value::Integer64(right)) => {
                            Value::Boolean(left != right)
                        }
                        (Value::Float32(left), Value::Float32(right)) => {
                            Value::Boolean(left != right)
                        }
                        (Value::Float64(left), Value::Float64(right)) => {
                            Value::Boolean(left != right)
                        }
                        (Value::Boolean(left), Value::Boolean(right)) => {
                            Value::Boolean(left != right)
                        }
                        (Value::String(left), Value::String(right)) => {
                            Value::Boolean(left != right)
                        }
                        (Value::Array(ArrayValue(left, _)), Value::Array(ArrayValue(right, _))) => {
                            Value::Boolean(left != right)
                        }
                        (Value::Slice(left), Value::Slice(right)) => {
                            Value::Boolean(left != right)
                        }
                        (Value::Tuple(left), Value::Tuple(right)) => {
                            Value::Boolean(left != right)
                        }
                        _ => panic!("You can't compare types for inequality other than integers, floats, booleans or strings"),
                    },
                    OrderComparison::LessThan => match (left, right) {
                        (Value::Integer32(left), Value::Integer32(right)) => {
                            Value::Boolean(left < right)
                        }
                        (Value::Integer64(left), Value::Integer64(right)) => {
                            Value::Boolean(left < right)
                        }
                        (Value::Float32(left), Value::Float32(right)) => Value::Boolean(left < right),
                        (Value::Float64(left), Value::Float64(right)) => Value::Boolean(left < right),
                        _ => panic!("You can't compare type order other than integers or floats"),
                    },
                    OrderComparison::LessThanOrEqual => match (left, right) {
                        (Value::Integer32(left), Value::Integer32(right)) => {
                            Value::Boolean(left <= right)
                        }
                        (Value::Float32(left), Value::Float32(right)) => Value::Boolean(left <= right),
                        (from, to) => panic!("You can't compare type order other than integers or floats (got {from:?} and {to:?})"),
                    },
                    OrderComparison::GreaterThan => match (left, right) {
                        (Value::Integer32(left), Value::Integer32(right)) => {
                            Value::Boolean(left > right)
                        }
                        (Value::Float32(left), Value::Float32(right)) => Value::Boolean(left > right),
                        _ => panic!("You can't compare type order other than integers or floats"),
                    },
                    OrderComparison::GreaterThanOrEqual => match (left, right) {
                        (Value::Integer32(left), Value::Integer32(right)) => {
                            Value::Boolean(left >= right)
                        }
                        (Value::Float32(left), Value::Float32(right)) => Value::Boolean(left >= right),
                        _ => panic!("You can't compare type order other than integers or floats"),
                    },
                })
    }

    fn evaluate_assignment(
        &mut self,
        value: &Value<'a>,
        variable_name: &NameHash,
    ) -> Result<Value<'a>> {
        match self.get_variable(variable_name) {
            Some(variable) => {
                if !variable.mutable {
                    let variable_name = self.ast.bihasher.unhash(variable_name);
                    bail!("Variable {variable_name:?} is not mutable");
                }
                let ValueReference::Mutable(ref variable_ref) = variable.value else {
                    unreachable!()
                };
                variable_ref.replace(value.clone());
            }
            None => {
                let variable_name = self.ast.bihasher.unhash(variable_name);
                bail!("Variable {variable_name:?} not found")
            }
        }

        Ok(Value::None)
    }

    fn evaluate_block_expression(&mut self, expression: &BlockExpression<'a>) -> Result<Value<'a>> {
        let BlockExpression(body) = expression;
        self.new_scope(ScopeKind::Block);
        self.evaluate_statement_body(body)?;
        self.pop_scope();
        Ok(Value::None)
    }

    fn evaluate_if_expression(&mut self, expression: &IfExpression<'a>) -> Result<Value<'a>> {
        let IfExpression(condition, body, else_body) = expression;
        let condition = self.evaluate_expression(condition)?;
        self.evaluate_if(&condition, body, else_body)
    }

    fn evaluate_if(
        &mut self,
        condition: &Value<'a>,
        body: &BlockExpression<'a>,
        else_body: &Option<Box<Expression<'a>>>,
    ) -> Result<Value<'a>> {
        extract_type_value!(condition, Boolean);

        Ok(if *condition {
            self.new_scope(ScopeKind::Block);
            let value = self.evaluate_block_expression(body)?;
            self.pop_scope();
            value
        } else if let Some(else_body) = else_body {
            self.new_scope(ScopeKind::Block);
            let value = self.evaluate_expression(else_body)?;
            self.pop_scope();
            value
        } else {
            Value::None
        })
    }

    fn evaluate_function_call_expression(
        &mut self,
        expression: &FunctionCallExpression<'a>,
    ) -> Result<Value<'a>> {
        let FunctionCallExpression(function, arguments) = expression;
        let function = self.evaluate_expression(function)?;
        self.evaluate_function_call(&function, arguments)
    }

    fn evaluate_call_to_function(
        &mut self,
        function: &FunctionValue<'a>,
        arguments: &Box<[Expression<'a>]>,
    ) -> Result<Value<'a>> {
        let FunctionValue(parameters, return_type, body) = function;

        if parameters.len() != arguments.len() {
            bail!("Argument count mismatch, got {arguments:?} but expected {parameters:?}");
        }

        self.new_scope(ScopeKind::Function(None));

        for (argument, (param_name, param_type)) in arguments.iter().zip(parameters.iter()) {
            let argument = self.evaluate_expression(argument)?;
            argument.assert_type_of(param_type)?;

            let scope = self.current_scope_mut();
            scope.set_immutable_variable(*param_name, argument);
        }

        self.evaluate_statement_body(body)?;

        let scope = self.pop_scope();
        Ok(if let ScopeKind::Function(Some(value)) = scope.kind {
            value.assert_type_of(return_type)?;
            value
        } else {
            Value::None
        })
    }

    fn evaluate_call_to_builtin_function(
        &mut self,
        builtin_function: &BuiltinFunctionValue<'a, 'a>,
        arguments: &Box<[Expression<'a>]>,
    ) -> Result<Value<'a>> {
        let BuiltinFunctionValue(parameters, variable_args, return_type, function) =
            builtin_function;

        if !variable_args && parameters.len() != arguments.len() {
            bail!("Argument count mismatch, got {arguments:?} but expected {parameters:?}");
        }

        let evaluated_arguments = if *variable_args {
            arguments
                .iter()
                .map(|argument| {
                    let argument = self.evaluate_expression(argument)?;
                    Ok(Cow::Owned(argument))
                })
                .collect::<Result<Box<[_]>>>()?
        } else {
            arguments
                .iter()
                .zip(parameters.iter())
                .map(|(argument, (_, param_type))| {
                    let argument = self.evaluate_expression(argument)?;
                    if *param_type != Type::Unknown {
                        argument.assert_type_of(param_type)?;
                    }
                    Ok(Cow::Owned(argument))
                })
                .collect::<Result<Box<[_]>>>()?
        };

        let value = function.borrow_mut()(evaluated_arguments)?;
        (&value).assert_type_of(return_type)?;
        Ok(value)
    }

    fn evaluate_function_call(
        &mut self,
        function: &Value<'a>,
        arguments: &Box<[Expression<'a>]>,
    ) -> Result<Value<'a>> {
        // call(...arguments)
        // fun dog(...parameters)

        let function = function.as_direct_ref();
        Ok(match function {
            Value::MutableReference(value) => {
                self.evaluate_function_call(&value.borrow(), arguments)?
            }
            Value::Function(function) => self.evaluate_call_to_function(function, arguments)?,
            Value::BuiltinFunction(builtin_function) => {
                self.evaluate_call_to_builtin_function(builtin_function, arguments)?
            }
            value => panic!("You can only call functions, you can't call {value:?}"),
        })
    }

    fn evaluate_while_loop(
        &mut self,
        condition: &Expression<'a>,
        body: &StatementBody<'a>,
    ) -> Result<()> {
        'outer: loop {
            let condition = self.evaluate_expression(condition)?;
            extract_type_value!(condition, Boolean);
            if !condition {
                break 'outer;
            }

            self.new_scope(ScopeKind::Loop(None));
            self.evaluate_statement_body(body)?;
            handle_loop_ending!(self, 'outer);
        }
        Ok(())
    }

    fn evaluate_loop(&mut self, body: &StatementBody<'a>) -> Result<()> {
        'outer: loop {
            self.new_scope(ScopeKind::Loop(None));
            self.evaluate_statement_body(&body)?;
            handle_loop_ending!(self, 'outer);
        }
        Ok(())
    }

    fn evaluate_inclusive_range<'b>(
        &mut self,
        from: Value<'a>,
        to: Value<'a>,
    ) -> Result<Value<'a>> {
        from.assert_type_of(&(&to).into())?;

        Ok(Value::InclusiveRange(InclusiveRangeValue(
            from.into(),
            to.into(),
        )))
    }

    fn evaluate_exclusive_range(&mut self, from: Value<'a>, to: Value<'a>) -> Result<Value<'a>> {
        from.assert_type_of(&(&to).into())?;

        Ok(Value::ExclusiveRange(ExclusiveRangeValue(
            from.into(),
            to.into(),
        )))
    }

    fn evaluate_for_loop<'b>(
        &mut self,
        iterable: &'b Value<'a>,
        pattern: &Pattern,
        body: &StatementBody<'a>,
    ) -> Result<()> {
        let iterable: Box<BuiltinIterator<'a, 'b>> = match iterable {
            Value::ExclusiveRange(range) => Box::new(range.into_iter()),
            Value::InclusiveRange(range) => Box::new(range.into_iter()),
            Value::Array(array) => Box::new(array.into_iter()),
            Value::Slice(items) => Box::new(items.into_iter()),
            value => unimplemented!("Iterables for {value:?} are not implemented"),
        };

        'outer: for value in iterable {
            self.new_scope(ScopeKind::Loop(None));
            self.evaluate_value_into_pattern(value, pattern, &mut None)?;
            self.evaluate_statement_body(body)?;
            handle_loop_ending!(self, 'outer);
        }

        Ok(())
    }

    fn evaluate_value_into_pattern<'b>(
        &mut self,
        value: Cow<'b, Value<'a>>,
        pattern: &Pattern,
        mutable: &mut Option<bool>,
    ) -> Result<()> {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Identifier(name_hash) => {
                self.current_scope_mut().set_variable(
                    *name_hash,
                    Variable::new(mutable.unwrap_or(false), value.into_owned()),
                );
            }
            Pattern::Mutable(pattern) => {
                if let Some(false) = mutable {
                    bail!(
                        "You can't enforce mutability on something that's specified as immutable"
                    );
                }

                mutable.replace(true);
                self.evaluate_value_into_pattern(value, pattern, mutable)?;
            }
            Pattern::Tuple(patterns) => {
                let value = value.into_owned();
                extract_type_value!(value, Tuple);
                let value = &value.0;

                if patterns.len() > value.len() {
                    bail!("Tuple pattern is longer than tuple value");
                }

                let mut p = 0;
                let len = value.len();
                let mut iter = value.iter().enumerate();

                while let Some((i, value)) = iter.next() {
                    let Some(pattern) = patterns.get(p) else {
                        break;
                    };
                    p += 1;

                    if let Pattern::Rest = pattern {
                        if i > patterns.len() {
                            bail!("Rest pattern can only appear once in array pattern");
                        }

                        // Move cursor to the last "length - remaining_patterns" elements
                        // Keep in mind that iterator consumes elements, thats why
                        // we have to subtract already evaluated patterns
                        // (p-1, because we add to p right after retrieving pattern)
                        let offset = len - patterns.len() + i;
                        // this saves us from underflowing :)
                        if offset >= p {
                            iter.nth(offset - p);
                        }
                        continue;
                    }

                    self.evaluate_value_into_pattern(Cow::Borrowed(value), pattern, mutable)?;
                }
            }
            Pattern::Array(patterns) => {
                let value = value.into_owned();
                extract_type_value!(value, Array);
                let ArrayValue(value, size) = value;

                if patterns.len() > *size {
                    bail!("Array pattern is longer than array value");
                }

                let mut p = 0;
                let mut iter = value.iter().enumerate();

                while let Some((i, value)) = iter.next() {
                    let Some(pattern) = patterns.get(p) else {
                        break;
                    };
                    p += 1;

                    if let Pattern::Rest = pattern {
                        if i > patterns.len() {
                            bail!("Rest pattern can only appear once in array pattern");
                        }

                        // Move cursor to the last "size - remaining_patterns" elements
                        // Keep in mind that iterator consumes elements, thats why
                        // we have to subtract already evaluated patterns
                        // (p-1, because we add to p right after retrieving pattern)
                        let offset = size - patterns.len() + i;
                        // this saves us from underflowing :)
                        if offset >= p {
                            iter.nth(offset - p);
                        }
                        continue;
                    }

                    self.evaluate_value_into_pattern(Cow::Borrowed(value), pattern, mutable)?;
                }
            }
            Pattern::Rest => {
                panic!("You can't have a rest pattern outside of array or tuple pattern");
            }
        }

        Ok(())
    }
}
