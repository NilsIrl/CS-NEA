use nom::Finish;
use std::{collections::HashMap, convert::TryInto, io, iter::zip, ops::Index};

use super::{
    parser::{
        program, Assignment, BigMistake, Call, Expression, FunctionDeclaration, ListOfStatements,
        ParseSettings, Reference, Statement,
    },
    value::{DenotedValue, Value},
};

#[derive(Debug)]
pub struct Error {}

#[derive(Debug, PartialEq)]
pub struct Program<'a>(pub ListOfStatements<'a>);

impl Program<'_> {
    pub fn from_str<'a>(
        input: &'a str,
        parse_settings: &ParseSettings,
    ) -> Result<Program<'a>, BigMistake<&'a str>> {
        program(parse_settings)(input)
            .finish()
            .map(|(_, program)| program)
    }

    pub fn interpret(self) {
        let mut context = Context {
            functions: HashMap::new(),
            classes: HashMap::new(),
            environment: vec![Default::default()],
            stdout: io::stdout(),
        };
        execute_statements(&self.0, &mut context);
    }

    pub fn interpret_with_write(self, stdout: impl io::Write) {
        let mut context = Context {
            functions: HashMap::new(),
            classes: HashMap::new(),
            environment: vec![Default::default()],
            stdout,
        };
        execute_statements(&self.0, &mut context);
    }
}

#[derive(Clone)]
struct Function<'a> {
    args: &'a Vec<(String, bool)>,
    body: &'a ListOfStatements<'a>,
}

#[derive(Clone)]
struct Method<'a> {
    function: Function<'a>,
    host_class: &'a str,
}

struct Class<'a> {
    fields: Vec<&'a str>,
    fields_in_scope: Vec<(usize, &'a str)>,
    methods: HashMap<&'a str, Method<'a>>,
    parent_class: Option<&'a str>,
}

#[derive(Default)]
struct StackFrame<'a> {
    variables: HashMap<&'a str, DenotedValue>,
    current_class: Option<String>,
}

impl<'a> StackFrame<'a> {
    fn get(&self, key: &str) -> Option<&DenotedValue> {
        self.variables.get(key)
    }

    fn insert(&mut self, key: &'a String, val: DenotedValue) -> Option<DenotedValue> {
        self.variables.insert(key, val)
    }
}

impl Index<&str> for StackFrame<'_> {
    type Output = DenotedValue;

    fn index(&self, index: &str) -> &Self::Output {
        &self.variables[index]
    }
}

type Environment<'a> = Vec<StackFrame<'a>>;

struct Context<'a, W: io::Write> {
    environment: Environment<'a>,
    classes: HashMap<&'a str, Class<'a>>,
    functions: HashMap<&'a str, Function<'a>>,
    stdout: W,
}

fn apply_env<'a>(
    identifier: &str,
    context: &mut Context<'a, impl io::Write>,
) -> Option<DenotedValue> {
    let env = &context.environment;
    env.last()
        .unwrap()
        .get(identifier)
        .or_else(|| env[0].get(identifier))
        .cloned()
}

fn get_ref(reference: &Reference, context: &mut Context<impl io::Write>) -> Option<DenotedValue> {
    match reference {
        Reference::Identifier(identifier) => apply_env(identifier, context),
        Reference::Index(reference, index) => {
            let array = eval(reference, context);
            let index = eval(index, context);
            Some(array.get_value_at_index(index))
        }
        Reference::ObjectAttribute(reference, attribute) => {
            let obj = eval(reference, context);
            match obj {
                Value::Object(class, values) => {
                    let class = &context.classes[class.as_str()];
                    let location_of_value = class
                        .fields_in_scope
                        .iter()
                        .find_map(|(index, attribute_in_class)| {
                            if attribute == attribute_in_class {
                                Some(index)
                            } else {
                                None
                            }
                        })
                        .expect("TODO: beterre error message, attribute does not exist on object");
                    Some(values[*location_of_value].clone())
                }
                val => panic!("{:?} is not an object", val),
            }
        }
    }
}

fn extend_env<'a>(
    reference: &'a Reference,
    value: Value,
    context: &mut Context<'a, impl io::Write>,
) -> DenotedValue {
    match get_ref(reference, context) {
        Some(denoted_value) => {
            denoted_value.replace(value);
            denoted_value
        }
        None => match reference {
            Reference::Identifier(k) => {
                let denoted_value = DenotedValue::from(value);
                context
                    .environment
                    .last_mut()
                    .unwrap()
                    .insert(k, denoted_value.clone());
                denoted_value
            }
            // TODO: improve error messages here
            Reference::Index(..) => {
                panic!("array does not exist");
            }
            Reference::ObjectAttribute(..) => {
                panic!("object does not exist")
            }
        },
    }
}

fn execute_statements<'a>(
    statements: &'a ListOfStatements<'a>,
    context: &mut Context<'a, impl io::Write>,
) {
    for statement in statements {
        match statement {
        Statement::Assignment(Assignment(reference, value)) => {
            let value = eval(value, context);
            extend_env(reference, value, context);
            //dbg!(&context.environment);
        }
        Statement::GlobalAssignment(name, value) => {
            let value = eval(value, context);
            context.environment[0].insert(name, DenotedValue::from(value));
        }
        Statement::ArrayDeclaration(ref name, dimensions) => {
            let dimensions: Vec<_> = dimensions
                .iter()
                .map(|size| eval(size, context).try_into().unwrap())
                .collect();
            context
                .environment
                .last_mut()
                .unwrap()
                .insert(name, DenotedValue::new_array_from_dimensions(&dimensions));
        }
        Statement::For(Assignment(var, begin), end, step, body) => {
            let counter = eval(begin, context);
            let end = eval(end, context);
            let step = eval(step, context);
            let step_is_positive_or_zero = step >= Value::from(0);
            let mut counter = extend_env(var, counter, context);

            while {
                step_is_positive_or_zero && *counter.borrow() <= end
                    || !step_is_positive_or_zero && *counter.borrow() >= end
            } {
                execute_statements(body, context);
                // This clone is cheap because it is an Rc<_>
                // However I feel like there must be a better solution
                //
                // The counter is also updated in the environment because the clone is an Rc
                counter += &step;
            }
            // TODO: We should probably remove the variable from the environment once we are done
        }
        Statement::If(exp, if_body, else_body) => {
            if eval(exp, context).try_into().unwrap() {
                execute_statements(if_body, context);
            } else {
                execute_statements(else_body, context);
            }
        }
        Statement::Expression(exp) => {
            eval(exp, context);
        }
        Statement::While(condition, body) => {
            while eval(condition, context).try_into().unwrap() {
                execute_statements(body, context);
            }
        }
        Statement::DoUntil(body, condition) => loop {
            execute_statements(body, context);
            if eval(condition, context).try_into().unwrap() {
                break;
            }
        },
        Statement::ClassDeclaration {
            name: class_name,
            superclass: parent_name,
            fields,
            methods,
        } => {
            let (mut new_methods, mut new_fields) = match parent_name {
                Some(parent_name) => (
                    context.classes[parent_name.as_str()].methods.clone(),
                    context.classes[parent_name.as_str()].fields.clone(),
                ),
                None => (HashMap::new(), Vec::new()),
            };
            // TODO: deal with private and public fields
            new_methods.extend(methods.into_iter().map(|(is_private, declaration)| {
                (
                    declaration.name.as_str(),
                    Method {
                        host_class: class_name,
                        function: Function {
                            args: &declaration.args,
                            body: &declaration.body,
                        },
                    },
                )
            }));
            new_fields.extend(fields.into_iter().map(|(is_private, name)| name.as_str()));
            let mut fields_in_scope: Vec<_> = new_fields.iter().map(|v| *v).enumerate().collect();
            fields_in_scope.sort_by_key(|a| a.1);
            fields_in_scope.dedup_by_key(|a| a.1);
            context.classes.insert(
                class_name,
                Class {
                    fields: new_fields,
                    methods: new_methods,
                    parent_class: parent_name.as_deref(),
                    fields_in_scope
                },
            );
        }
        Statement::Function(FunctionDeclaration { name, args, body }) => {
            context.functions.insert(name, Function { args, body });
        },
        Statement::Return(..) => unimplemented!("This should probably be removed anyway because a return statement is inherently something to evaluate, not a statement.")
    }
    }
}

fn print(args: &[DenotedValue], write: &mut impl io::Write) {
    // TODO: print on the same line
    for arg in args {
        // FIXME: Don't use format string here, we can just format and print directly
        // Using to_string
        writeln!(write, "{}", &*arg.borrow()).unwrap();
    }
}

fn apply_method<'a>(
    obj: DenotedValue,
    method: &str,
    super_call: bool,
    args: &Vec<Expression>,
    context: &mut Context<'a, impl io::Write>,
) -> Value {
    match &*obj.borrow() {
        Value::Object(class, field_values) => {
            let method = {
                let class = &context.classes[class.as_str()];
                if super_call {
                    let current_class = &context.classes[context
                        .environment
                        .last()
                        .unwrap()
                        .current_class
                        .as_ref()
                        .unwrap()
                        .as_str()];
                    // FIXME this calls the parent of the object we want the parent of the hosting
                    // class
                    context.classes[current_class.parent_class.unwrap()].methods[method].clone()
                } else {
                    class.methods[method].clone()
                }
            };
            let mut method_frame: HashMap<&str, DenotedValue> = context.classes[method.host_class]
                .fields_in_scope
                .iter()
                .map(|(field_location, field_name)| {
                    (*field_name, field_values[*field_location].clone())
                })
                .collect();
            method_frame.insert("self", obj.clone());
            method_frame.extend(zip(method.function.args, args).map(
                |((arg_name, as_ref), arg_val)| {
                    (
                        arg_name.as_str(),
                        if *as_ref {
                            match arg_val {
                                Expression::Reference(reference) => get_ref(reference, context)
                                    .expect(
                                        "TODO: improve error message if reference does not exist",
                                    ),
                                exp => panic!("{:?} is not a reference", exp),
                            }
                        } else {
                            DenotedValue::from(eval(arg_val, context))
                        },
                    )
                },
            ));
            context.environment.push(StackFrame {
                variables: method_frame,
                current_class: Some(method.host_class.to_string()),
            });
            execute_statements(method.function.body, context);
            context.environment.pop();
            // FIXME support function returning values
            Value::default()
        }
        _ => panic!("Cannot apply method on non object value"),
    }
}

// We should probably change this to return Value instead of DenotedValue
// This will prevent assigning to a non trivial expression as a thing
fn eval(expression: &Expression, context: &mut Context<impl io::Write>) -> Value {
    match expression {
        Expression::IntegerLiteral(integer) => Value::from(*integer),
        Expression::StringLiteral(str) => Value::from(*str),
        Expression::BoolLiteral(bool) => Value::from(*bool),
        Expression::FloatLiteral(float) => Value::from(*float),
        Expression::And(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);

            Value::from(lhs.try_into().unwrap() && rhs.try_into().unwrap())
        }
        Expression::Or(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);

            Value::from(lhs.try_into().unwrap() || rhs.try_into().unwrap())
        }
        Expression::Not(exp) => {
            Value::from(!TryInto::<bool>::try_into(eval(&*exp, context)).unwrap())
        }
        Expression::Equal(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);

            Value::from(lhs == rhs)
        }
        Expression::NotEqual(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);

            Value::from(lhs != rhs)
        }
        Expression::GreaterThan(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            Value::from(lhs > rhs)
        }
        Expression::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            Value::from(lhs >= rhs)
        }
        Expression::LessThan(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            Value::from(lhs < rhs)
        }
        Expression::LessThanOrEqual(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            Value::from(lhs <= rhs)
        }
        Expression::Plus(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            lhs + rhs
        }
        Expression::Minus(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            lhs - rhs
        }
        Expression::Modulus(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            lhs % rhs
        }
        Expression::Times(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);

            lhs * rhs
        }
        Expression::Divide(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            lhs / rhs
        }
        Expression::Quotient(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);

            match (lhs, rhs) {
                (Value::Integer(lhs), Value::Integer(rhs)) => Value::from(lhs / rhs),
                (lhs, rhs) => panic!("Can't calculate the quotient of {:?} and {:?}", lhs, rhs),
            }
        }
        Expression::Power(base, exponent) => {
            let base = eval(&*base, context);
            let exponent = eval(&*exponent, context);

            match (base, exponent) {
                (Value::Integer(lhs), Value::Integer(rhs)) => {
                    if rhs < 0 {
                        Value::from((lhs as f64).powi(rhs as i32))
                    } else {
                        Value::from(lhs.pow(rhs as u32))
                    }
                }
                (Value::Integer(base), Value::Float(exponent)) => {
                    Value::from((base as f64).powf(exponent))
                }
                (Value::Float(base), Value::Float(exponent)) => Value::from(base.powf(exponent)),
                (base, exponent) => panic!("Cannot take {:?} to the {:?}", base, exponent),
            }
        }
        // TODO: what to do if variable doesn't exist
        Expression::Reference(reference) => get_ref(reference, context).unwrap().borrow().clone(),
        Expression::Call(Call(rator, arguments)) => match &**rator {
            Expression::Reference(Reference::ObjectAttribute(obj, method)) => match &**obj {
                Expression::Reference(Reference::Identifier(identifier)) => {
                    if identifier == "super" {
                        let obj = context.environment.last().unwrap()["self"].clone();
                        apply_method(obj.clone(), &method, true, arguments, context)
                    } else {
                        apply_method(
                            apply_env(&identifier, context).unwrap(),
                            &method,
                            false,
                            arguments,
                            context,
                        )
                    }
                }
                Expression::Reference(reference) => apply_method(
                    get_ref(&reference, context).expect("TODO: if reference does not exist"),
                    &method,
                    false,
                    &arguments,
                    context,
                ),
                obj => {
                    let obj = eval(&obj, context);
                    apply_method(DenotedValue::from(obj), &method, false, arguments, context)
                }
            },
            Expression::Reference(Reference::Identifier(function_name)) => {
                let arguments: Vec<_> = arguments
                    .into_iter()
                    .map(|arg| DenotedValue::from(eval(arg, context)))
                    .collect();
                match function_name.as_str() {
                    "print" => {
                        print(&arguments, &mut context.stdout);
                        Value::NoVal
                    }
                    _ => todo!("Implement functions"),
                }
            }
            exp => panic!("Cannot call {:?}", exp),
        },
        Expression::New(Call(class_name, args)) => {
            if let Expression::Reference(Reference::Identifier(class_name)) = &**class_name {
                let obj = DenotedValue::from(Value::Object(
                    class_name.to_string(),
                    vec![
                        DenotedValue::from(Value::Undefined);
                        context.classes[class_name.as_str()].fields.len()
                    ],
                ));
                apply_method(obj.clone(), "new", false, args, context);
                //dbg!(&context.environment);
                let x = obj.borrow().clone();
                x
            } else {
                // TODO: improve error message
                panic!("Cannot create new instance of non class")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maths1_output_test() {
        Program::from_str(
            include_str!("../test_data/maths1.input"),
            &ParseSettings::default(),
        )
        .unwrap()
        .interpret()
    }

    macro_rules! output_test {
        ( $function_name:ident ) => {
            #[test]
            fn $function_name() {
                let mut stdout = Vec::new();
                Program::from_str(
                    include_str!(concat!(
                        "../test_data/",
                        stringify!($function_name),
                        ".input"
                    )),
                    &ParseSettings::default(),
                )
                .unwrap()
                .interpret_with_write(&mut stdout);
                print!("{}", std::str::from_utf8(&stdout).unwrap());
                assert_eq!(
                    stdout,
                    include_bytes!(concat!(
                        "../test_data/",
                        stringify!($function_name),
                        ".output"
                    ))
                );
            }
        };
    }

    output_test!(comment1);
    output_test!(switch1);
    output_test!(thinking_logically);
    output_test!(maths1_print);
    output_test!(fizzbuzz);
    output_test!(print_float1);
    output_test!(not_equal);
    output_test!(fizzbuzz_while_loop);
    output_test!(countdown_while_loop);
    output_test!(countdown_for_loop);
    output_test!(array_index1);
    output_test!(array_ocr_alevel_example);
    output_test!(chess_board_print);
    output_test!(class2);
    output_test!(bracket);
}
