use clam_common::ast::Type;
#[deny(dead_code)]
use clam_common::ast::Identifier;
use ordered_float::OrderedFloat;

use std::collections::HashMap;
use derive_more::{Add, Sub, Div, Mul, Not, Neg, From};

use crate::Ref;
use crate::Result;

pub enum ErrorSource {
    MismatchedTypes,
    DivisionByZero,
}

impl ErrorSource {
    pub fn empty_loc<T>(self) -> Result<T> {
        Err(ClamRuntimeError { loc: (0, 0), src: self })
    }
}

#[allow(dead_code)]
pub struct ClamRuntimeError {
    loc: (usize, usize),
    src: ErrorSource,
}

#[allow(dead_code)]
#[derive(PartialEq, Eq)]
pub struct ClamUserType {
    typename: Type,
    fields: HashMap<Identifier, Ref<ClamData>>,
}


#[derive(Clone, PartialEq, Eq, From)]
pub struct ClamString(String);
impl std::ops::Add for ClamString {
    type Output = ClamString;

    fn add(self, rhs: Self) -> Self::Output {
        ClamString(self.0 + &rhs.0)
    }
}


#[derive(PartialEq, Eq, Add, Sub, Mul, Div, Neg, From)]
pub struct ClamFloat(OrderedFloat<f64>);
#[derive(PartialEq, Eq, Add, Sub, Mul, Div, Neg, From)]
pub struct ClamInt(i64);
#[derive(PartialEq, Eq, Not, From)]
pub struct ClamBool(bool);

#[allow(dead_code)]
#[derive(PartialEq, Eq, From)]
pub enum ClamData {
    UserType(ClamUserType),
    String(ClamString),
    Float(ClamFloat),
    Int(ClamInt),
    Bool(ClamBool),
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
    type Output = Result<ClamData>;

    fn add(self, rhs: Self) -> Self::Output {
        use ClamData::*;

        if !self.type_eq(&rhs) {
            return Err(ClamRuntimeError::MismatchedTypes);
        }

        Ok(match (self, rhs) {
            (String(s1), String(s2)) => s1.clone() + s2,
            (Float(f1), Float(f2)) => f1 + f2,
            (Int(i1), Int(i2)) => i1 + i2,
            (Bool(b1), Bool(b2)) => todo!("Handle not applicable"),
            (UserType(u1), UserType(u2)) => todo!("operator on use type"),
            (_, _) => panic!("This branch should be unreachable"),
        }.into())
    }
}
