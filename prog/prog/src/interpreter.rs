use nom::Finish;
use rand::Rng;
use std::{
    cell::RefCell,
    collections::HashMap,
    convert::TryInto,
    fmt::Debug,
    fs::File,
    io::{self, BufRead, BufReader, Write},
    iter::{self, zip},
    ops::Index,
    rc::Rc,
};

use super::{
    parser::{
        program, Assignment, BigMistake, Call, Expression, FunctionDeclaration, ListOfStatements,
        ParseSettings, Reference, Statement,
    },
    value::{DenotedValue, Value},
};

#[cfg(unix)]
fn system(command: String) -> io::Result<String> {
    use lazy_static::lazy_static;
    use std::process::Command;
    use std::{env, ffi::OsString};

    lazy_static! {
        static ref SHELL: OsString = env::var_os("SHELL").unwrap();
    };

    Command::new(&*SHELL)
        .arg("-c")
        .arg(command)
        .output()
        .map(|output| String::from_utf8(output.stdout).unwrap())
}

#[cfg(windows)]
fn system(command: String) -> io::Result<String> {
    Command::new("cmd")
        .arg("/C")
        .arg(command)
        .output()
        .map(|output| String::from_utf8(output.stdout).unwrap())
}

#[cfg(all(
    target_arch = "wasm32",
    target_vendor = "unknown",
    target_os = "unknown"
))]
fn system(_: String) -> io::Result<String> {
    panic!("system unimplemented for web")
}

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
        let stdin = io::stdin();
        let mut context = Context {
            functions: HashMap::new(),
            classes: HashMap::new(),
            environment: vec![Default::default()],
            stdout: io::stdout(),
            stdin: stdin.lock(),
        };
        execute_statements(&self.0, &mut context);
    }

    pub fn interpret_with_io(self, stdout: impl Write + Debug, stdin: impl BufRead + Debug) {
        let mut context = Context {
            functions: HashMap::new(),
            classes: HashMap::new(),
            environment: vec![Default::default()],
            stdout,
            stdin,
        };
        execute_statements(&self.0, &mut context);
    }
}

#[derive(Clone, Debug)]
struct Function<'a> {
    args: &'a Vec<(String, bool)>,
    body: &'a ListOfStatements<'a>,
}

#[derive(Clone, Debug)]
struct Method<'a> {
    function: Function<'a>,
    host_class: &'a str,
}

#[derive(Debug)]
struct Class<'a> {
    field_count: usize,
    // field_index, is_private, field_name
    fields_in_scope: HashMap<&'a str, (usize, bool)>,
    methods: HashMap<&'a str, Method<'a>>,
    parent_class: Option<&'a str>,
}

#[derive(Debug, Default)]
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

#[derive(Debug)]
struct Context<'a, W: Write + Debug, R: BufRead + Debug> {
    environment: Environment<'a>,
    classes: HashMap<&'a str, Class<'a>>,
    functions: HashMap<&'a str, Function<'a>>,
    stdout: W,
    stdin: R,
}

fn apply_env(
    identifier: &str,
    context: &mut Context<impl Write + Debug, impl BufRead + Debug>,
) -> Option<DenotedValue> {
    let env = &context.environment;
    env.last()
        .unwrap()
        .get(identifier)
        .or_else(|| env[0].get(identifier))
        .cloned()
}

fn get_ref(
    reference: &Reference,
    context: &mut Context<impl Write + Debug, impl BufRead + Debug>,
) -> Option<DenotedValue> {
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
                    let field_data = context.classes[class.as_str()]
                        .fields_in_scope
                        .get(attribute.as_str())
                        .expect(&format!(
                            "field `{}` does not exist in `{}`",
                            attribute, &class
                        ));
                    if field_data.1 {
                        panic!("field `{}` is private and cannot be accessed from outside the class `{}`", attribute, &class);
                    }
                    Some(values[field_data.0].clone())
                }
                Value::String(string) => match attribute.as_str() {
                    "length" => Some(DenotedValue::from(Value::from(string.len() as i64))),
                    "upper" => Some(DenotedValue::from(Value::from(string.to_uppercase()))),
                    "lower" => Some(DenotedValue::from(Value::from(string.to_lowercase()))),
                    method_name => {
                        panic!("Cannot get attribute `{}` on string", method_name)
                    }
                },
                val => panic!("{:?} is not an object", val),
            }
        }
    }
}

fn extend_env<'a>(
    reference: &'a Reference,
    value: Value,
    context: &mut Context<'a, impl Write + Debug, impl BufRead + Debug>,
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

// executes the statements returning a value if a value is to be returned
// Otherwise continue
macro_rules! execute_statements {
    ($statements:expr, $context:expr) => {
        match execute_statements($statements, $context) {
            Value::Undefined => (),
            val => return val,
        }
    };
}

fn execute_statements<'a>(
    statements: &'a ListOfStatements<'a>,
    context: &mut Context<'a, impl Write + Debug, impl BufRead + Debug>,
) -> Value {
    for statement in statements {
        match statement {
            Statement::Assignment(Assignment(reference, value)) => {
                let value = eval(value, context);
                extend_env(reference, value, context);
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
                    execute_statements!(body, context);
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
                    execute_statements!(if_body, context);
                } else {
                    execute_statements!(else_body, context);
                }
            }
            Statement::Expression(exp) => {
                eval(exp, context);
            }
            Statement::While(condition, body) => {
                while eval(condition, context).try_into().unwrap() {
                    execute_statements!(body, context);
                }
            }
            Statement::DoUntil(body, condition) => loop {
                execute_statements!(body, context);
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
                let (mut new_methods, mut fields_in_scope, mut field_count) = match parent_name {
                    Some(parent_name) => (
                        context.classes[parent_name.as_str()].methods.clone(),
                        context.classes[parent_name.as_str()]
                            .fields_in_scope
                            .clone(),
                        context.classes[parent_name.as_str()].field_count,
                    ),
                    None => (HashMap::new(), HashMap::new(), 0),
                };
                // TODO: deal with private and public fields
                new_methods.extend(methods.into_iter().map(|(_is_private, declaration)| {
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
                for field in fields {
                    match fields_in_scope.get_mut(field.1.as_str()) {
                        Some(field_in_scope) => {
                            field_in_scope.0 = field_count;
                            field_in_scope.1 = field.0;
                        }
                        None => {
                            fields_in_scope.insert(field.1.as_str(), (field_count, field.0));
                        }
                    }
                    field_count += 1;
                }
                context.classes.insert(
                    class_name,
                    Class {
                        field_count,
                        methods: new_methods,
                        parent_class: parent_name.as_deref(),
                        fields_in_scope,
                    },
                );
            }
            Statement::Function(FunctionDeclaration { name, args, body }) => {
                context.functions.insert(name, Function { args, body });
            }
            Statement::Return(exp) => return eval(exp, context),
        }
    }
    Value::Undefined
}

fn assert_argument_count(method_name: &str, expected: usize, found: usize) {
    if expected != found {
        panic!(
            "`{}()` takes {} arguments(s) but {} were given",
            method_name, expected, found
        )
    }
}

fn apply_method<'a>(
    obj: DenotedValue,
    method: &str,
    super_call: bool,
    args: &Vec<Expression>,
    context: &mut Context<impl Write + Debug, impl BufRead + Debug>,
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
                    /*
                    context.classes[context.classes[current_class.methods[method].host_class]
                        .parent_class
                        .unwrap()]
                    .methods[method]
                        .clone()
                    */
                    // FIXME this calls the parent of the object we want the parent of the hosting
                    // class
                    // EDIT: this should actually be alright because the parent class is set as the
                    // host class
                    context.classes[current_class.parent_class.unwrap()].methods[method].clone()
                } else {
                    class.methods[method].clone()
                }
            };
            let mut method_frame: HashMap<&str, DenotedValue> = context.classes[method.host_class]
                .fields_in_scope
                .iter()
                .map(|(field_name, (field_location, _))| {
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
            let val = execute_statements(method.function.body, context);
            context.environment.pop();
            val
        }
        Value::String(string) => match method {
            "substring" => {
                if args.len() != 2 {
                    panic!(
                        "Wrong number of arguments passed to substring, expected 2 found {}",
                        args.len()
                    )
                }
                // 0 index is first element
                let starting_position: usize = eval(&args[0], context).try_into().unwrap();
                let number_of_characters: usize = eval(&args[1], context).try_into().unwrap();

                Value::from(&string[starting_position..(starting_position + number_of_characters)])
            }
            "left" => {
                if args.len() != 1 {
                    panic!(
                        "Wrong number of arguments passed to left, expected 1 found {}",
                        args.len()
                    )
                }

                let n: usize = eval(&args[0], context).try_into().unwrap();

                Value::from(&string[..n])
            }
            "right" => {
                if args.len() != 1 {
                    panic!(
                        "Wrong number of arguments passed to right, expected 1 found {}",
                        args.len()
                    )
                }

                let n: usize = eval(&args[0], context).try_into().unwrap();

                Value::from(&string[string.len() - n..])
            }
            method_name => {
                panic!("Cannot apply `{}` on string", method_name)
            }
        },
        Value::ReadFile(file) => match method {
            "readLine" => {
                let mut line = String::new();
                file.borrow_mut()
                    .as_mut()
                    .expect("File is closed")
                    .read_line(&mut line)
                    .unwrap();
                line.pop().unwrap();
                Value::from(line)
            }
            "endOfFile" => Value::from(
                !file
                    .borrow_mut()
                    .as_mut()
                    .expect("File is closed")
                    .has_data_left()
                    .unwrap(),
            ),
            "close" => {
                file.take();
                Value::Undefined
            }
            _ => panic!("No method {} on file", method),
        },
        Value::WriteFile(file) => match method {
            "writeLine" => {
                assert_argument_count("writeLine", 1, args.len());
                let line: String = eval(&args[0], context).try_into().unwrap();
                writeln!(
                    file.borrow_mut().as_ref().expect("File is closed"),
                    "{}",
                    line
                )
                .unwrap();
                Value::Undefined
            }
            "close" => {
                file.take();
                Value::Undefined
            }
            _ => panic!("No method {} on file", method),
        },
        _ => panic!("Cannot apply method on non object value"),
    }
}

// We should probably change this to return Value instead of DenotedValue
// This will prevent assigning to a non trivial expression as a thing
fn eval(
    expression: &Expression,
    context: &mut Context<impl Write + Debug, impl BufRead + Debug>,
) -> Value {
    match expression {
        Expression::IntegerLiteral(integer) => Value::from(*integer),
        Expression::StringLiteral(str) => Value::from(*str),
        Expression::BoolLiteral(bool) => Value::from(*bool),
        Expression::FloatLiteral(float) => Value::from(*float),
        Expression::NullLiteral => Value::Null,
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
                (Value::Float(base), Value::Integer(exponent)) => {
                    Value::from(base.powi(exponent as i32))
                }
                (base, exponent) => panic!("Cannot take {:?} to the {:?}", base, exponent),
            }
        }
        // TODO: what to do if variable doesn't exist
        Expression::Reference(reference) => get_ref(reference, context)
            .expect(&format!("Reference `{:?}` is not defined", reference))
            .borrow()
            .clone(),
        Expression::Call(Call(rator, arguments)) => match &**rator {
            Expression::Reference(Reference::ObjectAttribute(obj, method)) => match &**obj {
                Expression::Reference(Reference::Identifier(identifier)) => {
                    if identifier == "super" {
                        let obj = context.environment.last().unwrap()["self"].clone();
                        apply_method(obj, &method, true, arguments, context)
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
                match function_name.as_str() {
                    "print" => {
                        if arguments.len() != 1 {
                            panic!("expected 1 argument got {}", arguments.len())
                        }
                        let val = eval(&arguments[0], context);
                        writeln!(context.stdout, "{}", val).unwrap();
                        Value::Undefined
                    }
                    "input" => {
                        if arguments.len() > 1 {
                            panic!("expected 0 or 1 arguments got {}", arguments.len())
                        }
                        if let Some(argument) = arguments.get(0) {
                            let val = eval(argument, context);
                            write!(context.stdout, "{}", val).unwrap();
                            context.stdout.flush().unwrap();
                        }

                        let mut input_string = String::new();
                        context.stdin.read_line(&mut input_string).unwrap();
                        Value::String(input_string)
                    }
                    "ASC" => {
                        assert_argument_count("ASC", 1, arguments.len());
                        let v: String = eval(&arguments[0], context).try_into().unwrap();
                        Value::from(v.chars().next().unwrap() as i64)
                    }
                    "CHR" => {
                        assert_argument_count("CHR", 1, arguments.len());
                        let v: i64 = eval(&arguments[0], context).try_into().unwrap();
                        Value::from(char::from_u32(v as u32).unwrap().to_string())
                    }
                    "str" => {
                        assert_argument_count("str", 1, arguments.len());
                        eval(&arguments[0], context).cast_to_string()
                    }
                    "int" => {
                        assert_argument_count("int", 1, arguments.len());
                        eval(&arguments[0], context).cast_to_int()
                    }
                    "float" => {
                        assert_argument_count("float", 1, arguments.len());
                        eval(&arguments[0], context).cast_to_float()
                    }
                    "real" => {
                        assert_argument_count("real", 1, arguments.len());
                        eval(&arguments[0], context).cast_to_float()
                    }
                    "bool" => {
                        assert_argument_count("bool", 1, arguments.len());
                        eval(&arguments[0], context).cast_to_bool()
                    }
                    "random" => {
                        assert_argument_count("random", 2, arguments.len());
                        let start = eval(&arguments[0], context);
                        let end = eval(&arguments[1], context);
                        match (start, end) {
                            (Value::Float(start), Value::Float(end)) => {
                                Value::Float(rand::thread_rng().gen_range(start..=end))
                            }
                            (Value::Integer(start), Value::Integer(end)) => {
                                Value::Integer(rand::thread_rng().gen_range(start..=end))
                            }
                            (start, end) => panic!("cannot use random with {} and {}", start, end),
                        }
                    }
                    "openRead" => {
                        assert_argument_count("openRead", 1, arguments.len());
                        let filename: String = eval(&arguments[0], context).try_into().unwrap();
                        Value::ReadFile(Rc::new(RefCell::new(Some(BufReader::new(
                            File::open(&filename)
                                .expect(&format!("Failed to open file {}", filename)),
                        )))))
                    }
                    "openWrite" => {
                        assert_argument_count("openWrite", 1, arguments.len());
                        let filename: String = eval(&arguments[0], context).try_into().unwrap();
                        Value::WriteFile(Rc::new(RefCell::new(Some(
                            File::create(filename).unwrap(),
                        ))))
                    }
                    "system" => {
                        assert_argument_count("system", 1, arguments.len());
                        let command: String = eval(&arguments[0], context).try_into().unwrap();
                        Value::from(system(command).unwrap())
                    }
                    function_name => {
                        let function = context
                            .functions
                            .get(function_name)
                            .expect(&format!("function `{}` is not defined", function_name))
                            .clone();
                        let stack_frame = StackFrame {
                            variables: function
                                .args
                                .iter()
                                .zip(arguments.into_iter())
                                .map(|((arg_name, as_ref), arg_val)| {
                                    (
                                        arg_name.as_str(),
                                        if *as_ref {
                                            match arg_val {
                                                Expression::Reference(reference) => {
                                                    get_ref(reference, context)
                                                        .expect("TODO: put better error message")
                                                }
                                                exp => panic!("{:?} is not a reference", exp),
                                            }
                                        } else {
                                            DenotedValue::from(eval(arg_val, context))
                                        },
                                    )
                                })
                                .collect(),
                            current_class: None,
                        };
                        context.environment.push(stack_frame);
                        let val = execute_statements(function.body, context);
                        context.environment.pop();
                        val
                    }
                }
            }
            exp => panic!("Cannot call {:?}", exp),
        },
        Expression::New(Call(class_name, args)) => {
            if let Expression::Reference(Reference::Identifier(class_name)) = &**class_name {
                let obj = DenotedValue::from(Value::Object(
                    class_name.to_string(),
                    iter::repeat_with(|| DenotedValue::from(Value::Null))
                        .take(
                            context
                                .classes
                                .get(class_name.as_str())
                                .expect(&format!("class `{}` is not defined", &class_name))
                                .field_count,
                        )
                        .collect(),
                ));
                apply_method(obj.clone(), "new", false, args, context);
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

    #[test]
    #[should_panic]
    fn access_level() {
        let program = Program::from_str(
            include_str!("../test_data/access_level.input"),
            &ParseSettings::default(),
        )
        .unwrap();
        program.interpret();
    }

    #[test]
    #[should_panic]
    fn access_level_methods() {
        let program = Program::from_str(
            include_str!("../test_data/access_level_methods.input"),
            &ParseSettings::reject_single_quote(),
        )
        .unwrap();
        program.interpret();
    }

    macro_rules! output_test {
        ( $function_name:ident ) => {
            output_test!($function_name, &ParseSettings::default());
        };
        ( $function_name:ident, $parse_settings:expr ) => {
            #[test]
            fn $function_name() {
                let mut stdout = Vec::new();
                Program::from_str(
                    include_str!(concat!(
                        "../test_data/",
                        stringify!($function_name),
                        ".input"
                    )),
                    $parse_settings,
                )
                .unwrap()
                .interpret_with_io(&mut stdout, io::Cursor::new(Vec::new()));
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
    output_test!(countdown_while_loop, &ParseSettings::reject_single_quote());
    output_test!(countdown_for_loop, &ParseSettings::reject_single_quote());
    output_test!(array_index1);
    output_test!(array_ocr_alevel_example);
    output_test!(chess_board_print);
    output_test!(class2);
    output_test!(bracket);
    output_test!(print);
    output_test!(design_variables);
    #[cfg(unix)]
    output_test!(design_read_from_file);
    output_test!(for_loop1);
    output_test!(design_while);
    output_test!(design_string_handling);
    output_test!(design_subroutine_run);
    output_test!(design_seq);
    output_test!(design_byref);
    output_test!(precedence1);
    output_test!(j277_string_operations);
    output_test!(j277_builtin_functions);
    #[cfg(unix)]
    output_test!(copy_file);
    output_test!(return1);
    output_test!(return2);
    output_test!(return3);
    output_test!(class3);
    output_test!(class4);
    output_test!(logical_operators2);
    output_test!(access_level_methods1, &ParseSettings::reject_single_quote());
}
