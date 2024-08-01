#![allow(unused)]
use core::panic;
use std::fmt::Debug;

use crate::object::Object;

#[derive(Debug, PartialEq)]
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.val_type != other.val_type {
            return false;
        } else {
            match &self.data {
                ValData::ValNum(num) => match &other.data {
                    ValData::ValNum(other_num) => {
                        return num == other_num;
                    }
                    _ => return false,
                },
                ValData::ValBool(boolean) => match other.data {
                    ValData::ValBool(other_boolean) => {
                        return *boolean == other_boolean;
                    }
                    _ => return false,
                },
                ValData::ValObj(box_of_object) => match &other.data {
                    ValData::ValObj(other_box_of_object) => {
                        return box_of_object.get_type() == other_box_of_object.get_type();
                    }
                    _ => return false,
                },
            }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Value")
            .field("val_type", &self.val_type)
            .field("data", &self.data)
            .finish()
    }
}

impl Value {
    pub fn new(val_type: ValType, data: ValData) -> Self {
        Value { val_type, data }
    }

    pub fn from_num(num: i32) -> Self {
        Value {
            val_type: ValType::ValNumType,
            data: ValData::ValNum(num),
        }
    }
}

pub fn val_from_slice(val_type: ValType, slice: &str) -> Option<Value> {
    match val_type {
        ValType::ValNumType => {
            let maybe_number = slice.parse::<i32>();
            match maybe_number {
                Ok(num) => return Some(Value::from_num(num)),
                Err(msg) => return None,
            }
        }
        _ => None,
    }
}
