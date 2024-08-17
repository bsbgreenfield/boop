#![allow(unused)]
use core::panic;
use std::{fmt::Debug, rc::Rc};

use crate::object::{ObjString, Object};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValType {
    ValNumType,
    ValBoolType,
    ValStringType,
}

pub enum ValData {
    ValNum(i32),
    ValBool(bool),
    ValObj(Rc<dyn Object>), // this will almost certainly have to be an RC to account for
                            // interning strings
}

impl ValData {
    #[inline]
    pub fn unwrap_int(&self) -> i32 {
        if let ValData::ValNum(num) = self {
            *num
        } else {
            panic!("this is not a num type");
        }
    }

    #[inline]
    pub fn unwrap_bool(&self) -> bool {
        if let ValData::ValBool(boolean) = self {
            *boolean
        } else {
            panic!("This is not a bool type");
        }
    }

    pub fn unwrap_str(&self) -> &str {
        if let ValData::ValObj(obj_string_ref) = self {
            obj_string_ref.get_string()
        } else {
            panic!("you tried to unwrap string from an object that isnt a string...");
        }
    }
}

impl Clone for ValData {
    fn clone(&self) -> Self {
        match self {
            ValData::ValNum(num) => ValData::ValNum(*num),
            ValData::ValBool(boolean) => ValData::ValBool(*boolean),
            ValData::ValObj(obj) => ValData::ValObj(Rc::clone(obj)),
        }
    }
}

impl Debug for ValData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValData::ValNum(num) => write!(f, "{num}"),
            ValData::ValBool(boolean) => write!(f, "{boolean}"),
            ValData::ValObj(obj) => write!(f, "ValObj"),
        }
    }
}

pub struct Value {
    pub val_type: ValType,
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

    pub fn from_bool(boolean: bool) -> Self {
        Value {
            val_type: ValType::ValBoolType,
            data: ValData::ValBool(boolean),
        }
    }

    pub fn from_string(string: &str) -> Self {
        Value {
            val_type: ValType::ValStringType,
            data: ValData::ValObj(Rc::new(ObjString::new(string))),
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
