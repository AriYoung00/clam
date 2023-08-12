use clam_common::ast::Type;
#[deny(dead_code)]
use clam_common::ast::Identifier;
use ordered_float::OrderedFloat;

use std::collections::HashMap;
use std::sync::Arc;
use derive_more::{Add, Sub, Div, Mul, Not, Neg, From};

use crate::Result;

pub enum ErrorSource {
    DivisionByZero,
    VariableNotFound,
    FunctionNotFound,
}

impl ErrorSource {
    pub fn empty_loc<T>(self) -> Result<T> {
        Err(ClamRuntimeError { loc: (0, 0), src: self })
    }
    pub fn with_loc<T>(self, start: usize, end: usize) -> Result<T> {
        Err(ClamRuntimeError { loc: (start, end), src: self })
    }
}

#[allow(dead_code)]
pub struct ClamRuntimeError {
    loc: (usize, usize),
    src: ErrorSource,
}


#[allow(dead_code)]
#[derive(Debug)]
pub struct ClamUserType {
    typename: Type,
    fields: HashMap<Identifier, GcRef<ClamData>>,
}

impl PartialEq<Self> for ClamUserType {
    fn eq(&self, other: &Self) -> bool {
        self.typename == other.typename
    }
}
impl Eq for ClamUserType {}

enum GcStatus {
    Marked(Option<usize>),
    Unmarked,
}

pub struct Heap<T>(Vec<T>);
impl<T> Default for Heap<T> {
    fn default() -> Self {
        Self(Vec::with_capacity(1000))
    }
}

impl<T> Heap<T> {
    pub fn move_to_heap(&mut self, mut data: T) -> GcRef<T> {
        self.0.push(data);
        let idx = self.0.len() - 1;

        GcRef {
            idx,
            data: &mut self.0[idx] as *mut T,
        }
    }
}

#[derive(Debug)]
pub struct GcRef<T> {
    idx: usize,
    data: *mut T,
}

impl<T> AsRef<T> for GcRef<T> {
    fn as_ref(&self) -> &T {
        unsafe {
            &*self.data
        }
    }
}

impl<T> Clone for GcRef<T> {
    fn clone(&self) -> Self {
        Self { idx: self.idx, data: self.data }
    }
}
impl<T> Copy for GcRef<T> {}


#[derive(Clone, PartialEq, Eq, From, Debug)]
pub struct ClamString(pub Arc<str>);
impl std::ops::Add for &ClamString {
    type Output = ClamString;

    fn add(self, rhs: Self) -> Self::Output {
        ClamString((self.0.to_string() + &rhs.0).into())
    }
}


#[derive(PartialEq, Eq, Add, Sub, Mul, Div, Neg, From, Clone, Copy, Debug)]
pub struct ClamFloat(OrderedFloat<f64>);
#[derive(PartialEq, Eq, Add, Sub, Mul, Div, Neg, From, Clone, Copy, Debug)]
pub struct ClamInt(i64);
impl From<&i64> for ClamInt {
    fn from(value: &i64) -> Self {
        let value: i64 = *value;
        value.into()
    }
}


#[derive(PartialEq, Eq, Not, From, Clone, Copy, Debug)]
pub struct ClamBool(bool);

#[allow(dead_code)]
#[derive(PartialEq, Eq, From, Debug)]
pub enum ClamData {
    UserType(ClamUserType),
    String(ClamString),
    Float(ClamFloat),
    Int(ClamInt),
    Bool(ClamBool),
}

impl TryInto<ClamFloat> for &ClamData {
    type Error = ();

    fn try_into(self) -> std::result::Result<ClamFloat, Self::Error> {
        use ClamData::*;
        match self {
            Float(f) => Ok(ClamFloat(f.0)),
            Int(i) => Ok(ClamFloat((i.0 as f64).into())),
            _ => Err(()),
        }
    }
}

impl TryInto<ClamString> for &ClamData {
    type Error = ();

    fn try_into(self) -> std::result::Result<ClamString, Self::Error> {
        use ClamData::*;
        match self {
            String(s) => Ok(s.clone()),
            _ => Err(()),
        }
    }
}

impl ClamData {
    pub fn type_eq(&self, rhs: &Self) -> bool {
        use std::mem::discriminant;
        use ClamData::UserType;

        match (self, rhs) {
            (UserType(t1), UserType(t2)) => t1 == t2,
            (_, _) => discriminant(self) == discriminant(rhs),
        }
    }
}

impl std::ops::Add for &ClamData {
    type Output = ClamData;

    fn add(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        match (self, rhs) {
            (String(s1), String(s2)) => ClamData::String(s1 + s2),
            (Float(f1), Float(f2)) => ClamData::Float(*f1 + *f2),
            (Int(i1), Int(i2)) => ClamData::Int(*i1 + *i2),
            (Bool(b1), Bool(b2)) => unreachable!("bool + bool should be unreachable"),
            (UserType(u1), UserType(u2)) => unreachable!("cannot add user types"),

            (Float(f), other@_)
            | (other@_, Float(f))
                    if let Ok(rhs_) = other.try_into() => Float(*f + rhs_),

            (_, _) => unreachable!("mismatched type addition should be unreachable"),
        }.into()
    }
}

impl std::ops::Not for &ClamData {
    type Output = ClamData;

    fn not(self) -> Self::Output {
        match self {
            ClamData::Bool(ClamBool(b)) => ClamData::Bool(ClamBool(!b)),
            t@_ => unreachable!("logical not on something besides bool: {t:?}"),
        }
    }
}

impl std::ops::Sub for &ClamData {
    type Output = ClamData;

    fn sub(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        match (self, rhs) {
            (String(_), String(_)) => unreachable!("subtract strings should be unreachable"),
            (Float(f1), Float(f2)) => ClamData::Float(ClamFloat(f1.0 - f2.0)),
            (Int(i1), Int(i2)) => ClamData::Int(ClamInt(i1.0 - i2.0)),
            (_, _) => unreachable!("mismatched types in sub"),
        }
    }
}

impl std::ops::Mul for &ClamData {
    type Output = ClamData;

    fn mul(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        match (self, rhs) {
            (String(s1), String(s2)) => unreachable!("multiplication of strings should be unreachable"),
            (Float(f1), Float(f2)) => ClamData::Float(ClamFloat((*f1.0 * *f2.0).into())),
            (Int(i1), Int(i2)) => ClamData::Int(ClamInt(i1.0 * i2.0)),
            (Float(f), other@_)
            | (other@_, Float(f))
                    if let Ok(other_) = ClamFloat::try_from(*f) => Float(*f * other_.0),

            (_, _) => unreachable!("mismatched types in mul"),
        }
    }
}

impl std::ops::Div for &ClamData {
    type Output = Result<ClamData>;

    fn div(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        match (self, rhs) {
            (Float(f1), Float(f2)) => {
                if f2.0 == 0.0 {
                    ErrorSource::DivisionByZero.empty_loc()
                } else {
                    Ok(ClamData::Float(ClamFloat(f1.0 / f2.0)))
                }
            },
            (Int(i1), Int(i2)) => {
                if i2.0 == 0 {
                    ErrorSource::DivisionByZero.empty_loc()
                } else {
                    Ok(ClamData::Int(ClamInt(i1.0 / i2.0)))
                }
            },
            (String(_), String(_)) => unreachable!("divide strings should be unreachable"),
            (_, _) => unreachable!("mismatched types in div"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::data::{ClamData, ClamFloat};

    fn clam_float(f: f64) -> ClamData {
        ClamData::Float(ClamFloat(f.into()))
    }

    #[test]
    fn test_add() {
        assert_eq!(&clam_float(1.1) + &clam_float(2.4), clam_float(3.5));
    }
}
