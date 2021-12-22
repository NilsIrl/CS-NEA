use nom::Finish;
use std::{
    cell::RefCell,
    collections::HashMap,
    convert::TryInto,
    ops::{Add, AddAssign, Div, Rem},
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

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (lhs, rhs) => panic!("can't add {:?} and {:?}", lhs, rhs),
        }
    }
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

impl Rem for &Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs % rhs),
            (lhs, rhs) => panic!("can't calculate the remainder of {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Div for &Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs / rhs),
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
        let mut variables = Environment::with_capacity(1);
        variables.resize_with(1, Default::default);
        execute_statements(&self.0, &mut variables);
    }
}

#[derive(Debug)]
pub enum ProgError {
    IsNotBool,
}

type Environment<'a> = Vec<HashMap<&'a str, DenotedValue>>;

#[derive(Clone)]
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
        match *self.0.borrow() {
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

impl AddAssign for DenotedValue {
    fn add_assign(&mut self, rhs: Self) {
        match (&*self.0.borrow_mut(), &*rhs.0.borrow()) {
            (&Value::Integer(mut lhs), Value::Integer(rhs)) => lhs += rhs,
            (&Value::Float(mut lhs), Value::Float(rhs)) => lhs += rhs,
            // TODO: errors for each type
            _ => panic!("Can't add non-numeric values"),
        }
    }
}

impl Rem for DenotedValue {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::from(&*self.0.borrow() % &*rhs.0.borrow())
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
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs == rhs,
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

pub fn execute_statements<'a>(
    statements: &'a ListOfStatements<'a>,
    variables: &mut Environment<'a>,
) {
    for statement in statements {
        execute_statement(&statement, variables);
    }
}

fn execute_statement<'a>(statement: &'a Statement<'a>, variables: &mut Environment<'a>) {
    match statement {
        Statement::Assignment(Assignment(ref name, value)) => {
            let value = eval(value, variables);
            extend_env(variables, name, value);
        }
        Statement::For(var, beginning, end, step, body) => {
            let mut counter = eval(beginning, variables);
            let end = eval(end, variables);
            let step = eval(step, variables);

            while {
                step >= DenotedValue::from(0) && counter <= end
                    || step < DenotedValue::from(0) && counter >= end
            } {
                extend_env(variables, &var, counter.clone());
                execute_statements(body, variables);
                // This clone is sheep because it is an Rc<_>
                // However I feel like there must be a better solution
                counter += step.clone();
            }
        }
        Statement::If(exp, if_body, else_body) => {
            if eval(exp, variables).try_into().unwrap() {
                execute_statements(if_body, variables);
            } else {
                execute_statements(else_body, variables);
            }
        }
        Statement::Expression(exp) => {
            eval(exp, variables);
        }
        unimplemented_statement => todo!("Statement {:?} unimplemented", unimplemented_statement),
    }
}

fn print(args: impl Iterator<Item = DenotedValue>) {
    for arg in args {
        match &*arg.0.borrow() {
            Value::String(string) => println!("{}", string),
            val => unimplemented!("Printing {:?} unimplemented", val),
        }
    }
}

fn eval(expression: &Expression, variables: &mut Environment) -> DenotedValue {
    match expression {
        Expression::IntegerLiteral(integer) => DenotedValue::from(*integer),
        Expression::StringLiteral(str) => DenotedValue::from(*str),
        Expression::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = eval(&*lhs, variables);
            let rhs = eval(&*rhs, variables);
            DenotedValue::from(lhs >= rhs)
        }
        Expression::LessThanOrEqual(lhs, rhs) => {
            let lhs = eval(&*lhs, variables);
            let rhs = eval(&*rhs, variables);
            DenotedValue::from(lhs <= rhs)
        }
        Expression::Plus(lhs, rhs) => {
            let lhs = eval(&*lhs, variables);
            let rhs = eval(&*rhs, variables);
            lhs + rhs
        }
        Expression::Modulus(lhs, rhs) => {
            let lhs = eval(&*lhs, variables);
            let rhs = eval(&*rhs, variables);
            lhs % rhs
        }
        Expression::Divide(lhs, rhs) => {
            let lhs = eval(&*lhs, variables);
            let rhs = eval(&*rhs, variables);
            lhs / rhs
        }
        // TODO: what to do if variable doesn't exist
        Expression::Variable(name) => apply_env(variables, name.as_str()),
        Expression::FunctionCall(Call(function_name, arguments)) => {
            let arguments = arguments.into_iter().map(|arg| eval(arg, variables));
            match function_name.as_str() {
                "print" => {
                    print(arguments);
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
        let mut val2 = DenotedValue::from(Value::Integer(5));
        val1 += val2;
        assert_eq!(val1, DenotedValue::from(Value::Integer(7)));
    }
}
