use std::{
    cell::{Ref, RefCell, RefMut},
    convert::TryInto,
    fmt::Display,
    fs::File,
    io::BufReader,
    iter,
    ops::{Add, AddAssign, Div, Mul, Not, Rem, Sub},
    rc::Rc,
};

#[derive(Debug)]
pub enum ProgError {
    NotBool,
    NotPositiveInteger,
    NotArray,
    NotString,
}

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Array(Vec<DenotedValue>),

    // FIXME: change this to a reference instead of a String
    Object(String, Vec<DenotedValue>),

    ReadFile(Rc<RefCell<Option<BufReader<File>>>>),
    WriteFile(Rc<RefCell<Option<File>>>),

    // User facing null
    Null,
    // When a value is uninitialized (implementation detail)
    Undefined,
}

impl Value {
    pub fn get_value_at_index(&self, index: Value) -> DenotedValue {
        let index: usize = index.try_into().unwrap();
        match self {
            Value::Array(v) => v[index].clone(),
            _ => panic!("Cannot index {:?}", self),
        }
    }

    pub fn cast_to_bool(self) -> Self {
        Value::from(match self {
            Value::Integer(0) => false,
            Value::Integer(_) => true,
            Value::String(string) => {
                let string = string.to_lowercase();
                if string == "" || string == "false" {
                    false
                } else if string == "true" {
                    false
                } else {
                    panic!("Cannot cast {} to bool", string)
                }
            }
            Value::Boolean(b) => b,
            s => panic!("Cannot cast {} to bool", s),
        })
    }

    pub fn cast_to_int(self) -> Self {
        Value::from(match self {
            Value::Float(f) => f as i64,
            Value::String(s) => s.parse().unwrap(),
            Value::Integer(v) => v,
            Value::Boolean(b) => {
                if b {
                    1
                } else {
                    0
                }
            }
            v => panic!("Cannot cast {} to int", v),
        })
    }

    pub fn cast_to_string(self) -> Self {
        Value::from(match self {
            Value::Float(f) => f.to_string(),
            Value::Integer(i) => i.to_string(),
            Value::String(s) => s,
            Value::Boolean(b) => b.to_string(),
            v => panic!("Cannot cast {} to str", v),
        })
    }
    pub fn cast_to_float(self) -> Self {
        Value::from(match self {
            Value::String(s) => s.parse().unwrap(),
            Value::Integer(i) => i as f64,
            Value::Float(f) => f,
            v => panic!("Cannot cast {} to float", v),
        })
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Undefined
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Object(a1, a2), Value::Object(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
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

impl From<String> for Value {
    fn from(v: String) -> Self {
        Self::String(v)
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

impl TryInto<String> for Value {
    type Error = ProgError;

    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(ProgError::NotString),
        }
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
            (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 * rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs * rhs as f64),
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
            Self::Boolean(bool) => f.write_str(if *bool { "True" } else { "False" }),
            Self::Null => f.write_str("null"),
            v => panic!("Cannot display {:?}", v),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DenotedValue(pub Rc<RefCell<Value>>);

impl DenotedValue {
    pub fn new_array_from_dimensions(dimensions: &[usize]) -> Self {
        match dimensions {
            [] => Self::from(Value::Null),
            [x, xs @ ..] => Self::from(Value::Array(
                iter::repeat_with(|| Self::new_array_from_dimensions(xs))
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

impl From<Value> for DenotedValue {
    fn from(v: Value) -> Self {
        DenotedValue(Rc::new(RefCell::new(v)))
    }
}

impl AddAssign<&Value> for DenotedValue {
    fn add_assign(&mut self, other: &Value) {
        *self.0.borrow_mut() += other;
    }
}

impl PartialEq for DenotedValue {
    fn eq(&self, other: &Self) -> bool {
        &*self.0.borrow() == &*other.0.borrow()
    }
}

impl PartialOrd for DenotedValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (&*self.0.borrow()).partial_cmp(&*other.0.borrow())
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            (Value::String(lhs), Value::String(rhs)) => lhs.partial_cmp(rhs),
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs.partial_cmp(rhs),
            (Value::Array(lhs), Value::Array(rhs)) => lhs.partial_cmp(rhs),

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
        array.set_value_at_index(Value::from(3), DenotedValue::from(Value::from(4)));
        array.set_value_at_index(Value::from(4), DenotedValue::from(Value::from(5)));
        let val_at_index_3: i64 = array
            .get_value_at_index(Value::from(3))
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        assert_eq!(val_at_index_3, 4);
    }
}
