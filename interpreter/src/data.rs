use clam_common::ast::Type;
#[deny(dead_code)]
use clam_common::ast::Identifier;
use ordered_float::OrderedFloat;

use std::{collections::HashMap, cmp};
use std::sync::Arc;
use derive_more::{Add, Sub, Div, Mul, Not, Neg, From};

use crate::{Result, EvalResult};
use EvalResult::*;

#[derive(Debug)]
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
#[derive(Debug)]
pub struct ClamRuntimeError {
    pub loc: (usize, usize),
    pub src: ErrorSource,
}


#[allow(dead_code)]
#[derive(Debug)]
pub struct ClamUserType {
    typename: Type,
    fields: HashMap<Identifier, ClamData>,
}

impl PartialEq<Self> for ClamUserType {
    fn eq(&self, other: &Self) -> bool {
        self.typename == other.typename
    }
}
impl Eq for ClamUserType {}

#[allow(dead_code)]
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

#[allow(dead_code)]
impl<T> Heap<T> {
    pub fn move_to_heap(&mut self, data: T) -> GcRef<T> {
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

impl<T> PartialEq for GcRef<T>
where T: PartialEq {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            self.idx == other.idx && (*self.data) == (*other.data)
        }
    }
}
impl <T> Eq for GcRef<T> where T: PartialEq {}

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
impl std::ops::Add for ClamString {
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
pub struct ClamBool(pub bool);

#[allow(dead_code)]
#[derive(PartialEq, Eq, From, Debug, Clone)]
pub enum ClamData {
    UserType(GcRef<ClamUserType>),
    String(ClamString),
    Float(ClamFloat),
    Int(ClamInt),
    Bool(ClamBool),
    Empty,
}


impl TryFrom<&ClamData> for ClamFloat {
    type Error = ();

    fn try_from(value: &ClamData) -> std::result::Result<Self, Self::Error> {
        use ClamData::*;
        use core::result::Result::*;
        match value {
            Float(f) => Ok(ClamFloat(f.0)),
            Int(i) => Ok(ClamFloat((i.0 as f64).into())),
            _ => Err(()),
        }
    }
}

impl TryFrom<ClamData> for ClamFloat {
    type Error = ();

    fn try_from(value: ClamData) -> std::result::Result<Self, Self::Error> {
        ClamFloat::try_from(&value)
    }
}

impl TryInto<ClamString> for ClamData {
    type Error = ();

    fn try_into(self) -> std::result::Result<ClamString, Self::Error> {
        use core::result::Result::*;
        use ClamData::*;
        match self {
            String(s) => Ok(s.clone()),
            _ => Err(()),
        }
    }
}


#[allow(dead_code)]
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

impl std::ops::Add for ClamData {
    type Output = ClamData;

    fn add(self, rhs: Self) -> Self::Output {
        use ClamData::*;
        use core::result::Result::*;

        match (self, rhs) {
            (String(s1), String(s2)) => ClamData::String(s1 + s2),
            (Float(f1), Float(f2)) => ClamData::Float(f1 + f2),
            (Int(i1), Int(i2)) => ClamData::Int(i1 + i2),
            (Bool(_b1), Bool(_b2)) => unreachable!("bool + bool should be unreachable"),
            (UserType(_u1), UserType(_u2)) => unreachable!("cannot add user types"),

            (Float(f), other@_)
            | (other@_, Float(f))
                    if let Ok(rhs_) = ClamFloat::try_from(&other) => Float(f + rhs_),

            (_, _) => unreachable!("mismatched type addition should be unreachable"),
        }.into()
    }
}

impl std::ops::Not for ClamData {
    type Output = ClamData;

    fn not(self) -> Self::Output {
        match self {
            ClamData::Bool(ClamBool(b)) => ClamData::Bool(ClamBool(!b)),
            ClamData::Int(ClamInt(i)) => ClamData::Int(ClamInt(!i)),
            t@_ => unreachable!("logical not on something besides bool: {t:?}"),
        }
    }
}

impl std::ops::Neg for ClamData {
    type Output = ClamData;

    fn neg(self) -> Self::Output {
        match self {
            ClamData::Float(f1) => ClamData::Float(-f1),
            ClamData::Int(i1) => ClamData::Int(-i1),
            other => unreachable!("negating '{other:?} should be unreachable"),
        }
    }
}

impl std::ops::Sub for ClamData {
    type Output = ClamData;

    fn sub(self, rhs: Self) -> Self::Output {
        use ClamData::*;
        use core::result::Result::*;

        match (self, rhs) {
            (String(_), String(_)) => unreachable!("subtract strings should be unreachable"),
            (Float(f1), Float(f2)) => ClamData::Float(ClamFloat(f1.0 - f2.0)),
            (Int(i1), Int(i2)) => ClamData::Int(ClamInt(i1.0 - i2.0)),
            (Float(f1), other)
              if let Ok(other) = ClamFloat::try_from(&other) => ClamData::Float(ClamFloat(f1.0 - other.0)),
            (other, Float(f2))
              if let Ok(other) = ClamFloat::try_from(&other) => ClamData::Float(ClamFloat(other.0 - f2.0)),
            (_, _) => unreachable!("mismatched types in sub"),
        }
    }
}

impl std::ops::Mul for ClamData {
    type Output = ClamData;

    fn mul(self, rhs: Self) -> Self::Output {
        use ClamData::*;
        use core::result::Result::*;

        match (self, rhs) {
            (String(_), String(_)) => unreachable!("multiplication of strings should be unreachable"),
            (Float(f1), Float(f2)) => ClamData::Float(ClamFloat((*f1.0 * *f2.0).into())),
            (Int(i1), Int(i2)) => ClamData::Int(ClamInt(i1.0 * i2.0)),
            (Float(f), other)
            | (other, Float(f))
                    if let Ok(other_) = ClamFloat::try_from(&other) => Float(f * other_.0),

            (_, _) => unreachable!("mismatched types in mul"),
        }
    }
}

fn try_div<InType, CombType, CombOut>(lhs: InType, rhs: InType, comb: CombType) -> Result<CombOut>
where 
  InType: std::ops::Div + num_traits::Zero,
  CombType: FnOnce(<InType as std::ops::Div>::Output) -> CombOut {
    if rhs.is_zero() {
        ErrorSource::DivisionByZero.empty_loc()
    }
    else {
        Ok(comb(lhs / rhs))
    }
}
fn compose<F, G, I1, I2, O2>(f: F, g: G) -> impl Fn(I1) -> O2
where
    F: Fn(I2) -> O2,
    G: Fn(I1) -> I2,
{
    move |x| f(g(x))
}


impl std::ops::Div for ClamData {
    type Output = Result<ClamData>;

    fn div(self, rhs: Self) -> Self::Output {
        use ClamData::*;
        use core::result::Result::*;

        match (self, rhs) {
            (Float(f1), Float(f2)) => 
                try_div(f1.0, f2.0, compose(ClamData::Float, ClamFloat)),
            (Int(i1), Int(i2)) =>
                try_div(i1.0, i2.0, compose(ClamData::Int, ClamInt)),
            (Float(f1), other)
              if let Ok(other) = ClamFloat::try_from(&other) =>
                try_div(f1.0, other.0, compose(ClamData::Float, ClamFloat)),
            (other, Float(f2))
              if let Ok(other) = ClamFloat::try_from(&other) =>
                try_div(other.0, f2.0, compose(ClamData::Float, ClamFloat)),
            (String(_), String(_)) => unreachable!("divide strings should be unreachable"),
            (_, _) => unreachable!("mismatched types in div"),
        }
    }
}

fn try_mod<InType, CombType, CombOut>(lhs: InType, rhs: InType, comb: CombType) -> Result<CombOut>
where 
  InType: std::ops::Rem + num_traits::Zero,
  CombType: FnOnce(<InType as std::ops::Rem>::Output) -> CombOut {
    if rhs.is_zero() {
        ErrorSource::DivisionByZero.empty_loc()
    }
    else {
        Ok(comb(lhs % rhs))
    }
}

impl std::ops::Rem for ClamData {
    type Output = Result<ClamData>;

    fn rem(self, rhs: Self) -> Self::Output {
        use ClamData::*;
        use core::result::Result::*;

        let as_float = compose(ClamData::Float, ClamFloat);
        let as_int = compose(ClamData::Int, ClamInt);

        match (self, rhs) {
            (Float(f1), Float(f2)) => try_mod(f1.0, f2.0, as_float),
            (Int(i1), Int(i2)) => try_mod(i1.0, i2.0, as_int),
            (Float(f1), other) if let Ok(other) = ClamFloat::try_from(&other) => try_mod(f1.0, other.0, as_float),
            (other, Float(f2)) if let Ok(other) = ClamFloat::try_from(&other) => try_mod(other.0, f2.0, as_float),
            (lhs, rhs) => unreachable!("invalid operands to modulus: '{lhs:?} % {rhs:?}' should be unreachable"),
        }
    }
}

impl PartialOrd for ClamData {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        use ClamData::*;
        use core::result::Result::*;

        match (self, other) {
            (Float(f1), Float(f2)) => Some(f1.0.cmp(&f2.0)),
            (Int(i1), Int(i2)) => Some(i1.0.cmp(&i2.0)),
            (Float(f1), other) if let Ok(other) = ClamFloat::try_from(other) => Some(f1.0.cmp(&other.0)),
            (other, Float(f2)) if let Ok(other) = ClamFloat::try_from(other) => Some(other.0.cmp(&f2.0)),
            (lhs, rhs) => unreachable!("invalid operands to comparison op: '{lhs:?} % {rhs:?}' should be unreachable"),
        }
    }
}

impl std::ops::BitAnd for ClamData {
    type Output = ClamData;

    fn bitand(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        match (self, rhs) {
            (Int(ClamInt(i1)), Int(ClamInt(i2))) => Int(ClamInt(i1 & i2)),
            (Bool(ClamBool(b1)), Bool(ClamBool(b2))) => Bool(ClamBool(b1 & b2)),
            (lhs, rhs) => unreachable!("invalid operands to bitand: '{lhs:?} & {rhs:?}' should be unreachable"),
        }
    }
}

impl std::ops::BitOr for ClamData {
    type Output = ClamData;

    fn bitor(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        match (self, rhs) {
            (Int(ClamInt(i1)), Int(ClamInt(i2))) => Int(ClamInt(i1 | i2)),
            (Bool(ClamBool(b1)), Bool(ClamBool(b2))) => Bool(ClamBool(b1 | b2)),
            (lhs, rhs) => unreachable!("invalid operands to bitor: '{lhs:?} & {rhs:?}' should be unreachable"),
        }
    }
}

impl std::ops::BitXor for ClamData {
    type Output = ClamData;

    fn bitxor(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        match (self, rhs) {
            (Int(ClamInt(i1)), Int(ClamInt(i2))) => Int(ClamInt(i1 ^ i2)),
            (Bool(ClamBool(b1)), Bool(ClamBool(b2))) => Bool(ClamBool(b1 ^ b2)),
            (lhs, rhs) => unreachable!("invalid operands to bitxor: '{lhs:?} & {rhs:?}' should be unreachable"),
        }
    }
}


#[cfg(test)]
mod test {
    use crate::data::{ClamData, ClamFloat};

    use super::ClamInt;

    fn cfloat(f: f64) -> ClamData {
        ClamData::Float(ClamFloat(f.into()))
    }

    fn cint(i: i64) -> ClamData {
        ClamData::Int(ClamInt(i))
    }

    #[test]
    fn test_add() {
        assert_eq!(cfloat(4.0) + cfloat(2.0), cfloat(6.0));
        assert_eq!(cfloat(4.0) + cint(2), cfloat(6.0));
        assert_eq!(cint(4) + cfloat(2.0), cfloat(6.0));
        assert_eq!(cint(10) + cint(2), cint(12));
    }
    
    #[test]
    fn test_sub() {
        assert_eq!(cfloat(4.0) - cfloat(2.0), cfloat(2.0));
        assert_eq!(cfloat(4.0) - cint(2), cfloat(2.0));
        assert_eq!(cint(4)     - cfloat(2.0), cfloat(2.0));
        assert_eq!(cint(10)    - cint(2), cint(8));
    }
    
    #[test]
    fn test_mul() {
        assert_eq!(cfloat(4.0) * cfloat(2.0), cfloat(8.0));
        assert_eq!(cfloat(4.0) * cint(2), cfloat(8.0));
        assert_eq!(cint(4)     * cfloat(2.0), cfloat(8.0));
        assert_eq!(cint(10)    * cint(2), cint(20));
    }
    

    #[test]
    fn test_div() {
        assert_eq!((cfloat(4.0) / cfloat(2.0)).unwrap(), cfloat(2.0));
        assert_eq!((cfloat(4.0) / cint(2)).unwrap(), cfloat(2.0));
        assert_eq!((cint(4)     / cfloat(2.0)).unwrap(), cfloat(2.0));
        assert_eq!((cint(10)    / cint(2)).unwrap(), cint(5));
    }

}
