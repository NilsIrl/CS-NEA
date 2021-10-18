use std::{
    collections::HashMap,
    convert::TryInto,
    ops::{Add, AddAssign, Div, Rem},
};

use super::parser::{Expression, ListOfStatements, Statement};

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

impl TryInto<f64> for Value {
    type Error = Error;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Value::Float(float) => Ok(float),
            Value::Integer(integer) => Ok(integer as f64),
            _ => Err(Error {}),
        }
    }
}

impl TryInto<i64> for Value {
    type Error = Error;

    fn try_into(self) -> Result<i64, Self::Error> {
        if let Value::Integer(integer) = self {
            Ok(integer)
        } else {
            // TODO more meaningful errors
            Err(Error {})
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            Value::Integer(lhs) => match other {
                Value::Integer(rhs) => lhs.partial_cmp(rhs),
                Value::Float(rhs) => (*lhs as f64).partial_cmp(rhs),
                _ => unimplemented!(),
            },
            Value::Float(lhs) => match other {
                Value::Integer(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                Value::Float(rhs) => lhs.partial_cmp(rhs),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Integer(lhs) => match rhs {
                Self::Integer(rhs) => Self::Integer(lhs + rhs),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl Add for &mut Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Value::Integer(lhs) => match rhs {
                Value::Integer(rhs) => Value::Integer(*lhs + *rhs),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Value::from(
            TryInto::<f64>::try_into(self).unwrap() / TryInto::<f64>::try_into(rhs).unwrap(),
        )
    }
}

impl Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Value::from(
            TryInto::<i64>::try_into(self).unwrap() % TryInto::<i64>::try_into(rhs).unwrap(),
        )
    }
}

impl AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        match self {
            Self::Integer(lhs) => match rhs {
                Self::Integer(rhs) => *self = Self::Integer(*lhs + rhs),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> AddAssign<&'a Value> for Value {
    fn add_assign(&mut self, rhs: &Self) {
        match self {
            Self::Integer(lhs) => match rhs {
                Self::Integer(rhs) => *self = Self::Integer(*lhs + rhs),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Integer(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::String(v.to_string())
    }
}

pub type Environment<'a> = HashMap<&'a str, Value>;

pub fn execute_statements<'a>(statements: &ListOfStatements<'a>, variables: &mut Environment<'a>) {
    for statement in statements {
        execute_statement(statement, variables);
    }
}

fn execute_statement<'a>(statement: &Statement<'a>, variables: &mut Environment<'a>) {
    match statement {
        Statement::For(var, beginning, end, step, body) => {
            {
                let beginning = eval(beginning, variables);
                variables.insert(var, beginning);
            }
            let end = eval(end, variables);
            let step = eval(step, variables);

            while {
                let counter = variables.get(var).unwrap();
                step >= Value::Integer(0) && *counter <= end
                    || step < Value::Integer(0) && *counter >= end
            } {
                execute_statements(body, variables);

                // TODO: The variables counter could have been deleted in some for or another
                *variables.get_mut(var).unwrap() += &step;
            }
        }
        Statement::If(exp, if_body, else_body) => {
            if let Value::Boolean(boolean_value) = eval(exp, variables) {
                if boolean_value {
                    execute_statements(if_body, variables);
                } else {
                    execute_statements(else_body, variables);
                }
            } else {
                todo!("Not a boolean");
            }
        }
        Statement::Expression(exp) => {
            eval(exp, variables);
        }
        unimplemented_statement => todo!("Statement {:?} unimplemented", unimplemented_statement),
    }
}

fn print(args: impl Iterator<Item = Value>) {
    for arg in args {
        match arg {
            Value::String(string) => println!("{}", string),
            val => unimplemented!("Printing {:?} unimplemented", val),
        }
    }
}

fn eval(expression: &Expression, variables: &mut Environment) -> Value {
    match expression {
        Expression::IntegerLiteral(integer) => Value::from(*integer),
        Expression::StringLiteral(str) => Value::from(*str),
        Expression::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = eval(lhs, variables);
            let rhs = eval(rhs, variables);
            Value::from(lhs >= rhs)
        }
        Expression::LessThanOrEqual(lhs, rhs) => {
            let lhs = eval(lhs, variables);
            let rhs = eval(rhs, variables);
            Value::from(lhs <= rhs)
        }
        Expression::Plus(lhs, rhs) => {
            let lhs = eval(lhs, variables);
            let rhs = eval(rhs, variables);
            lhs + rhs
        }
        Expression::Modulus(lhs, rhs) => {
            let lhs = eval(lhs, variables);
            let rhs = eval(rhs, variables);
            lhs % rhs
        }
        // TODO: what to do if variable doesn't exist
        Expression::Variable(name) => variables.get(name).unwrap().clone(),
        Expression::Divide(lhs, rhs) => {
            let lhs = eval(lhs, variables);
            let rhs = eval(rhs, variables);
            lhs / rhs
        }
        Expression::FunctionCall(function_name, arguments) => {
            let arguments = arguments.iter().map(|arg| eval(arg, variables));
            match *function_name {
                "print" => {
                    print(arguments);
                    Value::NoVal
                }
                _ => todo!("Implement functions"),
            }
        }
        unimplemented_expression => {
            todo!("Expression {:?} unimplemented", unimplemented_expression)
        }
    }
}
