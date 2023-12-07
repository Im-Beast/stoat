use miette::{bail, Error};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer32,
    Integer64,
    Float32,
    Float64,
    Boolean,
    String,

    InclusiveRange,
    ExclusiveRange,

    Function(Box<[Type]>, Box<Type>), // parameters, return_type

    // Built-in objects
    Array(Box<Type>, usize), // type, size
    Slice(Box<Type>),
    Tuple(Box<[Type]>),

    // Custom type like struct or enum
    Custom(String), // type_name

    // Can only be returned by functions, cannot be applied to variables
    None,
    Unknown, // Used for builtins
}

#[derive(Debug)]
pub struct TupleType(Vec<Type>);

impl TryFrom<&str> for Type {
    type Error = Error;
    fn try_from(type_str: &str) -> Result<Self, Self::Error> {
        let type_str = type_str.trim();

        let value_type = match type_str {
            "i32" => Type::Integer32,
            "i64" => Type::Integer64,
            "f32" => Type::Float32,
            "f64" => Type::Float64,
            "bool" => Type::Boolean,
            "str" => Type::String,
            "" => Type::None,
            _ if type_str.ends_with("[]") => {
                // Parsing slice type
                // Parses strings like:
                //  - type[]
                // where type can be any type, including another slice

                let type_str = &type_str[0..type_str.len() - 3];
                let value_type: Type = type_str.try_into()?;
                Type::Slice(Box::new(value_type))
            }
            _ if type_str.len() > 3 && type_str.ends_with(']') => {
                // minimal type of array is 4 chars: t[0]
                // Parsing array type
                // Parses strings like:
                //  - type[size]
                // where type can be any type, including another array
                // and size is a number

                let Some(opening_bracket) = type_str.rfind('[') else {
                    bail!("Incorrent array type syntax (missing opening bracket \"[\")")
                };

                let size_str = &type_str[opening_bracket + 1..type_str.len() - 1];
                let size = if let Ok(size) = size_str.parse::<usize>() {
                    size
                } else {
                    bail!(
                        "Incorrent array type syntax (failed parsing size of an array): {}, size_str: {}",
                        type_str, size_str
                    )
                };

                let items_type_str = &type_str[0..opening_bracket];
                let items_type: Type = items_type_str.try_into()?;

                Type::Array(Box::new(items_type), size)
            }
            _ if type_str.starts_with('(') => {
                // Parsing Tuple type
                // Parses strings like:
                //  - (type, type2, type3)
                // Tuples can contain other tuples, so type can look like:
                //  - (type, (type2, type3), type4)
                //  - ((type1, type2), type3)

                // check if final cursor == len() otherwise there's something more to do (e.g. arrays)

                let mut stack: Vec<TupleType> = Vec::with_capacity(2);
                let mut final_tuple_type: Option<Type> = None;

                let mut accum_start = 0;
                let mut accum_end = 0;

                let mut i = 0;
                for char in type_str.chars() {
                    match char {
                        '(' => stack.push(TupleType(Vec::new())),
                        ')' => {
                            if accum_start > 0 {
                                let type_str = &type_str[accum_start..accum_end];
                                let item_type: Type = type_str.try_into()?;
                                let last_i = stack.len() - 1;
                                let last_tuple = &mut stack[last_i];
                                last_tuple.0.push(item_type);
                                accum_start = 0;
                            }

                            let Some(tuple_type) = stack.pop() else {
                                bail!(
                                    "Incorrent tuple type syntax (last stack item missing): {}",
                                    type_str
                                );
                            };

                            let tuple_type = Type::Tuple(tuple_type.0.into());

                            if stack.is_empty() {
                                final_tuple_type = Some(tuple_type);
                                break;
                            } else {
                                let last_i = stack.len() - 1;
                                let last_tuple = &mut stack[last_i];
                                last_tuple.0.push(tuple_type);
                            }
                        }
                        ',' => {
                            if accum_start > 0 {
                                let type_str = &type_str[accum_start..accum_end];
                                let item_type: Type = type_str.try_into()?;
                                let last_i = stack.len() - 1;
                                let last_tuple = &mut stack[last_i];
                                last_tuple.0.push(item_type);

                                accum_start = 0;
                            }

                            accum_end = i + 1;
                        }
                        _ => {
                            if accum_start == 0 {
                                accum_start = i;
                            }
                            accum_end = i + 1;
                        }
                    }

                    i += 1;
                }

                if let Some(final_tuple_type) = final_tuple_type {
                    final_tuple_type
                } else {
                    bail!(
                        "Incorrent tuple type syntax (final tuple type missing): {}",
                        type_str
                    )
                }
            }
            _ => Type::Custom(type_str.to_string()),
        };

        Ok(value_type)
    }
}
