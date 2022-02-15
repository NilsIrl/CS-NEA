use std::{
    cell::{Ref, RefCell, RefMut},
    convert::TryInto,
    fmt::Display,
    iter,
    ops::{Add, AddAssign, Div, Mul, Not, Rem, Sub},
    rc::Rc,
};

#[derive(Debug)]
pub enum ProgError {
    NotBool,
    NotPositiveInteger,
    NotArray,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Array(Vec<DenotedValue>),

    // FIXME: change this to a reference instead of a String
    Object(String, Vec<DenotedValue>),

    // When a value is uninitialized
    Undefined,

    // to deal with functions vs procedures
    NoVal,
}

impl Value {
    pub fn get_value_at_index(&self, index: Value) -> DenotedValue {
        let index: usize = index.try_into().unwrap();
        match self {
            Value::Array(v) => v[index].clone(),
            _ => panic!("Cannot index {:?}", self),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Undefined
    }
}

impl Not for &Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(true) => Value::Boolean(false),
            Value::Boolean(false) => Value::Boolean(true),
            _ => panic!("Cannot compute logical negation of {:?}", self),
        }
    }
}

impl AddAssign<&Value> for Value {
    fn add_assign(&mut self, other: &Self) {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => *lhs += rhs,
            (Value::Float(lhs), Value::Float(rhs)) => *lhs += rhs,
            (lhs, rhs) => panic!("can't add_assign {:?} and {:?}", lhs, rhs),
        };
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (Value::String(lhs), Value::String(rhs)) => Value::String(format!("{}{}", lhs, rhs)),
            (lhs, rhs) => panic!("can't add {:?} and {:?}", lhs, rhs),
        }
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

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Integer(v)
    }
}

impl TryInto<bool> for Value {
    type Error = ProgError;

    fn try_into(self) -> Result<bool, Self::Error> {
        match self {
            Value::Boolean(b) => Ok(b),
            _ => Err(ProgError::NotBool),
        }
    }
}

impl TryInto<i64> for Value {
    type Error = ProgError;
    fn try_into(self) -> Result<i64, Self::Error> {
        match self {
            Value::Integer(s) => Ok(s),
            _ => Err(ProgError::NotPositiveInteger),
        }
    }
}

impl TryInto<usize> for Value {
    type Error = ProgError;
    fn try_into(self) -> Result<usize, Self::Error> {
        TryInto::<i64>::try_into(self).map(|v| v as usize)
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
            (lhs, rhs) => panic!("can't substract {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs % rhs),
            (lhs, rhs) => panic!("can't calculate the remainder of {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            (lhs, rhs) => panic!("Can't multiply {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Float(lhs as f64 / rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
            (lhs, rhs) => panic!("Can't divide {:?} and {:?}", lhs, rhs),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int) => write!(f, "{}", int),
            Self::Float(float) => write!(f, "{}", float),
            Self::String(str) => f.write_str(str),
            v => panic!("Cannot display {:?}", v),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DenotedValue(pub Rc<RefCell<Value>>);

impl DenotedValue {
    pub fn new_array_from_dimensions(dimensions: &[usize]) -> Self {
        match dimensions {
            [] => Self::from(Value::Undefined),
            [x, xs @ ..] => Self::from(Value::Array(
                iter::from_fn(|| Some(Self::new_array_from_dimensions(xs)))
                    .take(*x)
                    .collect(),
            )),
        }
    }

    pub fn copy(&self) -> Self {
        Self::from(self.borrow().clone())
    }

    pub fn borrow(&self) -> Ref<'_, Value> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, Value> {
        self.0.borrow_mut()
    }

    pub fn get_value_at_index(&self, index: Value) -> DenotedValue {
        let index: usize = index.try_into().unwrap();
        match &*self.0.borrow() {
            Value::Array(v) => v[index].clone(),
            _ => panic!("Cannot index {:?}", self),
        }
    }

    pub fn set_value_at_index(&self, index: Value, value: DenotedValue) {
        let index: usize = index.try_into().unwrap();
        match *self.0.borrow_mut() {
            Value::Array(ref mut v) => v[index] = value,
            _ => panic!("Cannot index {:?}", self),
        }
    }

    pub fn replace(&self, v: Value) {
        self.0.replace(v);
    }
}

impl Default for DenotedValue {
    fn default() -> Self {
        DenotedValue::from(Value::Undefined)
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
        match *self.0.borrow() {
            Value::Boolean(b) => Ok(b),
            _ => Err(ProgError::NotBool),
        }
    }
}

impl Not for DenotedValue {
    type Output = DenotedValue;

    fn not(self) -> Self::Output {
        Self::from(!&*self.0.borrow())
    }
}

impl AddAssign<&Value> for DenotedValue {
    fn add_assign(&mut self, other: &Value) {
        *self.0.borrow_mut() += other;
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => lhs.partial_cmp(&(*rhs as f64)),

            (lhs, rhs) => panic!("Cannot compare {:?} and {:?}", lhs, rhs),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_assign_test1() {
        let mut val1 = DenotedValue::from(Value::Integer(2));
        let val2 = Value::Integer(5);
        val1 += &val2;
        assert_eq!(val1, DenotedValue::from(Value::Integer(7)));
        val1 += &val2;
        assert_eq!(val1, DenotedValue::from(Value::Integer(12)));
    }

    #[test]
    fn new_array_test() {
        let array = DenotedValue::new_array_from_dimensions(&[10]);
        array.set_value_at_index(Value::from(3), DenotedValue::from(4));
        array.set_value_at_index(Value::from(4), DenotedValue::from(5));
        let val_at_index_3: i64 = array
            .get_value_at_index(Value::from(3))
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        assert_eq!(val_at_index_3, 4);
    }
}
