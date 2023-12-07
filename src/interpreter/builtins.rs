use std::{borrow::Cow, cell::RefCell, rc::Rc};

use rustc_hash::FxHasher;

use crate::{
    extract_type_value, hash,
    interpreter::values::BuiltinFunctionValue,
    shared::{name_bihasher::NameHash, types::Type},
};

use super::values::Value;
use miette::{bail, Result};

#[macro_export]
macro_rules! builtin_function {
    ($name:ident($($arg_name:ident:$arg_type:ident),* / $variable_args:expr) -> $return_type:ident, |$closure_args:ident| $closure_body:expr) => {{
        let args = Box::new([$({
            (hash!(FxHasher, stringify!($arg_name)), Type::$arg_type)
        },)*]);

        let builtin_func: Builtin = (
            hash!(FxHasher, stringify!($name)),
            Value::BuiltinFunction(BuiltinFunctionValue(
                args,
                $variable_args,
                Type::$return_type,
                Rc::new(RefCell::new(|$closure_args: Box<[Cow<Value>]>| $closure_body)),
            )),
        );

        builtin_func
    }};

    ($name:ident($($arg_name:ident:$arg_type:ident),*, ...) -> $return_type:ident, |$closure_args:ident| $closure_body:expr) => {{
        builtin_function!($name($($arg_name:$arg_type),* / true) -> $return_type, |$closure_args| $closure_body)
    }};

    ($name:ident($($arg_name:ident:$arg_type:ident),*) -> $return_type:ident, |$closure_args:ident| $closure_body:expr) => {{
        builtin_function!($name($($arg_name:$arg_type),* / false) -> $return_type, |$closure_args| $closure_body)
    }};
}

pub type Builtin<'a, 'b> = (NameHash, Value<'a>);

pub fn format_template<T: ToString>(template: T, values: &[Cow<Value>]) -> Result<String> {
    let mut template = template.to_string();

    let mut values = values.iter();

    while template.contains("{}") {
        let value = values.next();
        if let Some(value) = value {
            template = template.replacen("{}", &value.to_string(), 1);
        } else {
            bail!("Not enough values to format template")
        }
    }

    Ok(template)
}

pub fn get_builtins<'a, 'b>() -> Vec<Builtin<'a, 'b>> {
    let print = builtin_function!(
        print(template:String, value:Unknown, ...) -> None,
        |args| {
            let template = &args[0];
            extract_type_value!(template, String);
            print!("{}\n", format_template(template, &args[1..])?);
            Ok(Value::None)
        }
    );

    let format = builtin_function!(
        format(template:String, value:Unknown, ...) -> String,
        |args| {
            let template = &args[0];
            extract_type_value!(template, String);
            let formatted = format_template(template, &args[1..])?;
            Ok(Value::String(formatted))
        }
    );

    let assert = builtin_function!(
        assert(value:Boolean, message:String) -> None,
        |args| {
            let value = &args[0];
            extract_type_value!(value, Boolean);

            let message = &args[1];
            extract_type_value!(message, String);

            if *value {
                Ok(Value::None)
                } else {
                    bail!("{}", message);
            }
        }
    );

    vec![print, format, assert]
}
