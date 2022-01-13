use std::{
    cell::{Ref, RefCell},
    convert::TryInto,
    ops::{Add, Div, Mul, Rem, Sub},
    rc::Rc,
};

#[derive(Debug)]
pub enum ProgError {
    IsNotBool,
}

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

#[derive(Clone, Debug)]
pub struct DenotedValue(Rc<RefCell<Value>>);

impl DenotedValue {
    pub fn borrow(&self) -> Ref<'_, Value> {
        self.0.borrow()
    }
}

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
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            //(Value::Integer(lhs), Value::Float(rhs)) => *lhs as f64 == *rhs,
            //(Value::Float(lhs), Value::Integer(rhs)) => *lhs == *rhs as f64,
            // TODO: How should we consider NoVal?
            (lhs, rhs) => panic!("Cannot compare {:?} and {:?}", lhs, rhs),
        }
    }
}

impl PartialOrd for DenotedValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (&*self.0.borrow(), &*other.0.borrow()) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => lhs.partial_cmp(&(*rhs as f64)),

            (lhs, rhs) => panic!("Cannot compare {:?} and {:?}", lhs, rhs),
        }
    }
}
