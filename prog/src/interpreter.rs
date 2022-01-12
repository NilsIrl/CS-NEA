use nom::Finish;
use std::{
    cell::RefCell,
    collections::HashMap,
    convert::TryInto,
    io,
    ops::{Add, Div, Mul, Rem, Sub},
    rc::Rc,
};

use super::parser::{
    program, Assignment, Call, Expression, ListOfStatements, ParseSettings, Statement,
};

#[derive(Debug)]
pub struct Error {}

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    NoVal,
}

impl Add for &Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (lhs, rhs) => panic!("can't add {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Sub for &Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
            (lhs, rhs) => panic!("can't substract {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Rem for &Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs % rhs),
            (lhs, rhs) => panic!("can't calculate the remainder of {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Mul for &Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            (lhs, rhs) => panic!("Can't multiply {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Div for &Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Float(*lhs as f64 / *rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
            (lhs, rhs) => panic!("Can't divide {:?} and {:?}", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program<'a>(pub ListOfStatements<'a>);

impl Program<'_> {
    pub fn from_str<'a>(
        input: &'a str,
        parse_settings: &ParseSettings,
    ) -> Result<Program<'a>, nom::error::Error<&'a str>> {
        program(parse_settings)(input)
            .finish()
            .map(|(_, program)| program)
    }

    pub fn interpret(self) {
        let mut context = Context {
            environment: vec![Default::default()],
            stdout: io::stdout(),
        };
        execute_statements(&self.0, &mut context);
    }

    pub fn interpret_with_write(self, stdout: impl io::Write) {
        let mut context = Context {
            environment: vec![Default::default()],
            stdout,
        };
        execute_statements(&self.0, &mut context);
    }
}

#[derive(Debug)]
pub enum ProgError {
    IsNotBool,
}

type Environment<'a> = Vec<HashMap<&'a str, DenotedValue>>;

struct Context<'a, W: io::Write> {
    environment: Environment<'a>,
    stdout: W,
}

#[derive(Clone, Debug)]
pub struct DenotedValue(Rc<RefCell<Value>>);

impl From<bool> for DenotedValue {
    fn from(v: bool) -> Self {
        DenotedValue(Rc::new(RefCell::new(Value::Boolean(v))))
    }
}

impl From<i64> for DenotedValue {
    fn from(v: i64) -> Self {
        DenotedValue(Rc::new(RefCell::new(Value::Integer(v))))
    }
}

impl From<f64> for DenotedValue {
    fn from(v: f64) -> Self {
        DenotedValue(Rc::new(RefCell::new(Value::Float(v))))
    }
}

impl From<&str> for DenotedValue {
    fn from(v: &str) -> Self {
        DenotedValue(Rc::new(RefCell::new(Value::String(v.to_string()))))
    }
}

impl From<Value> for DenotedValue {
    fn from(v: Value) -> Self {
        DenotedValue(Rc::new(RefCell::new(v)))
    }
}

impl TryInto<bool> for DenotedValue {
    type Error = ProgError;
    fn try_into(self) -> Result<bool, Self::Error> {
        match *(*self.0).borrow() {
            Value::Boolean(b) => Ok(b),
            _ => Err(ProgError::IsNotBool),
        }
    }
}

impl Add for DenotedValue {
    type Output = DenotedValue;

    fn add(self, rhs: Self) -> Self::Output {
        Self::from(&*self.0.borrow() + &*rhs.0.borrow())
    }
}

impl Sub for DenotedValue {
    type Output = DenotedValue;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::from(&*self.0.borrow() - &*rhs.0.borrow())
    }
}

impl Rem for DenotedValue {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::from(&*self.0.borrow() % &*rhs.0.borrow())
    }
}

impl Mul for DenotedValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::from(&*self.0.borrow() * &*rhs.0.borrow())
    }
}

impl Div for DenotedValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::from(&*self.0.borrow() / &*rhs.0.borrow())
    }
}

impl PartialEq for DenotedValue {
    fn eq(&self, other: &Self) -> bool {
        match (&*self.0.borrow(), &*other.0.borrow()) {
            (Value::Integer(ref lhs), Value::Integer(ref rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            // TODO: How should we consider NoVal?
            _ => false,
        }
    }
}

impl PartialOrd for DenotedValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (&*self.0.borrow(), &*other.0.borrow()) {
            (Value::Integer(lhs), Value::Integer(ref rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(ref rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

fn extend_env<'a>(env: &mut Environment<'a>, name: &'a str, value: DenotedValue) {
    env.last_mut().unwrap().insert(name, value);
}

fn apply_env(env: &mut Environment, name: &str) -> DenotedValue {
    match env.last().unwrap().get(name) {
        Some(val) => val.clone(),
        None => env[0].get(name).unwrap().clone(),
    }
}

fn execute_statements<'a>(
    statements: &'a ListOfStatements<'a>,
    context: &mut Context<'a, impl io::Write>,
) {
    for statement in statements {
        execute_statement(&statement, context);
    }
}

fn execute_statement<'a>(statement: &'a Statement<'a>, context: &mut Context<'a, impl io::Write>) {
    match statement {
        Statement::Assignment(Assignment(ref name, value)) => {
            let value = eval(value, context);
            extend_env(&mut context.environment, name, value);
        }
        Statement::For(var, beginning, end, step, body) => {
            let mut counter = eval(beginning, context);
            let end = eval(end, context);
            let step = eval(step, context);

            while {
                step >= DenotedValue::from(0) && counter <= end
                    || step < DenotedValue::from(0) && counter >= end
            } {
                extend_env(&mut context.environment, &var, counter.clone());
                execute_statements(body, context);
                // This clone is sheep because it is an Rc<_>
                // However I feel like there must be a better solution
                counter = counter + step.clone();
                //counter += step.clone();
            }
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
        unimplemented_statement => todo!("Statement {:?} unimplemented", unimplemented_statement),
    }
}

fn print(args: &[DenotedValue], write: &mut impl io::Write) {
    for arg in args {
        match &*arg.0.borrow() {
            Value::String(string) => writeln!(write, "{}", string).unwrap(),
            val => unimplemented!("Printing {:?} unimplemented", val),
        }
    }
}

fn eval(expression: &Expression, context: &mut Context<impl io::Write>) -> DenotedValue {
    match expression {
        Expression::IntegerLiteral(integer) => DenotedValue::from(*integer),
        Expression::StringLiteral(str) => DenotedValue::from(*str),
        Expression::Equal(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);

            DenotedValue::from(lhs == rhs)
        }
        Expression::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            DenotedValue::from(lhs >= rhs)
        }
        Expression::LessThanOrEqual(lhs, rhs) => {
            let lhs = eval(&*lhs, context);
            let rhs = eval(&*rhs, context);
            DenotedValue::from(lhs <= rhs)
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

            let x = DenotedValue::from(match (&*lhs.0.borrow(), &*rhs.0.borrow()) {
                (Value::Integer(lhs), Value::Integer(rhs)) => lhs / rhs,
                _ => panic!("Can't calculate the quotient of {:?} and {:?}", lhs, rhs),
            });
            x
        }
        Expression::Power(base, exponent) => {
            let base = eval(&*base, context);
            let exponent = eval(&*exponent, context);

            let x = match (&*base.0.borrow(), &*exponent.0.borrow()) {
                (Value::Integer(lhs), Value::Integer(rhs)) => {
                    if *rhs < 0 {
                        DenotedValue::from((*lhs as f64).powi(*rhs as i32))
                    } else {
                        DenotedValue::from(lhs.pow(*rhs as u32))
                    }
                }
                (Value::Integer(base), Value::Float(exponent)) => {
                    DenotedValue::from((*base as f64).powf(*exponent))
                }
                (Value::Float(base), Value::Float(exponent)) => {
                    DenotedValue::from(base.powf(*exponent))
                }
                (base, exponent) => panic!("Cannot take {:?} to the {:?}", base, exponent),
            };
            x
        }
        // TODO: what to do if variable doesn't exist
        Expression::Variable(name) => apply_env(&mut context.environment, name.as_str()),
        Expression::FunctionCall(Call(function_name, arguments)) => {
            let arguments: Vec<_> = arguments
                .into_iter()
                .map(|arg| eval(arg, context))
                .collect();
            match function_name.as_str() {
                "print" => {
                    print(&arguments, &mut context.stdout);
                    DenotedValue::from(Value::NoVal)
                }
                _ => todo!("Implement functions"),
            }
        }
        unimplemented_expression => {
            todo!("Expression {:?} unimplemented", unimplemented_expression)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_assign_test1() {
        let mut val1 = DenotedValue::from(Value::Integer(2));
        let val2 = DenotedValue::from(Value::Integer(5));
        val1 = val1 + val2;
        assert_eq!(val1, DenotedValue::from(Value::Integer(7)));
    }

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
}
