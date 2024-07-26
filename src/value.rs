#![allow(unused)]
use std::fmt::Debug;

use crate::object::Object;

#[derive(Debug)]
pub enum ValType {
    ValNumType,
    ValBoolType,
}

pub enum ValData {
    ValNum(i32),
    ValBool(bool),
    ValObj(Box<dyn Object>), // this will almost certainly have to be an RC to account for
                             // interning strings
}

impl Debug for ValData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValData::ValNum(num) => f.debug_struct("ValNum").field("num", num).finish(),
            ValData::ValBool(boolean) => {
                f.debug_struct("ValBool").field("boolean", boolean).finish()
            }
            ValData::ValObj(obj) => write!(f, "ValObj"),
        }
    }
}

pub struct Value {
    val_type: ValType,
    pub data: ValData,
}

impl Value {
    pub fn new(val_type: ValType, data: ValData) -> Self {
        Value { val_type, data }
    }
}
