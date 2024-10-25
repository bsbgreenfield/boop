#![allow(unused)]
use core::panic;
use std::{fmt::Debug, rc::Rc};

use crate::object::{ObjString, Object};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValType {
    ValNumType,
    ValBoolType,
    ValStringType,
    ValFunctionType,
}


// TODO: make this more efficient by either making another hash set, 
// implementing an incremental search, or hardcoding these valtypes in the Tokens.
impl ValType {
    pub fn from_str(type_lexeme: &str) -> Self {
        match type_lexeme {
           "String" => ValType::ValStringType,
           "int" => ValType::ValNumType,
           "bool" => ValType::ValBoolType,
           _ => panic!("unimplemented"),
        }
    }
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

    pub fn print_value(&self) {
        match self {
            ValData::ValNum(num) => println!("{num}"),
            ValData::ValBool(boolean) => println!("{boolean}"),
            ValData::ValObj(obj) => match obj.get_type() {
                crate::object::ObjType::ObjStringType => {
                    println!("{}", obj.get_string());
                }
                _ => panic!("printing has not yet been implemented for this type!"),
            },
        }
    }

    pub fn compare_value(&self, other: &Self) -> bool {
        match self {
            ValData::ValNum(num) => *num == other.unwrap_int(),
            ValData::ValBool(boolean) => *boolean == other.unwrap_bool(),
            ValData::ValObj(obj) => Self::compare_objs(obj, other),
        }
    }

    fn compare_objs(obj: &Rc<dyn Object>, other: &Self) -> bool {
        match obj.get_type() {
            crate::object::ObjType::ObjStringType => Self::compare_strings(obj.get_string(), other),
            crate::object::ObjType::ObjBobjType => panic!("have implemented obj comparison yet"),
        }
    }

    fn compare_strings(string_1: &str, other: &Self) -> bool {
        match other {
            ValData::ValObj(obj) => match obj.get_type() {
                crate::object::ObjType::ObjStringType => {
                    return string_1 == obj.get_string();
                }
                _ => panic!("Cannot compare a string with another object"),
            },
            _ => panic!("cannot compare a string with a num or a bool"),
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
            ValData::ValObj(obj) => debug_print_obj(f, obj),
        }
    }
}

fn debug_print_obj(f: &mut std::fmt::Formatter<'_>, obj: &Rc<dyn Object>) -> std::fmt::Result {
    match obj.get_type() {
        crate::object::ObjType::ObjStringType => {
            let string = obj.get_string();
            write!(f, "{string}")
        }
        _ => write!(f, "ValObj"),
    }
}

pub struct Value {
    pub val_type: ValType,
    pub data: ValData,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.val_type != other.val_type {
            false
        } else {
            match &self.data {
                ValData::ValNum(num) => match &other.data {
                    ValData::ValNum(other_num) => {
                        num == other_num
                    }
                    _ => false,
                },
                ValData::ValBool(boolean) => match other.data {
                    ValData::ValBool(other_boolean) => {
                        *boolean == other_boolean
                    }
                    _ => false,
                },
                ValData::ValObj(box_of_object) => match &other.data {
                    ValData::ValObj(other_box_of_object) => {
                        box_of_object.get_type() == other_box_of_object.get_type()
                    }
                    _ => false,
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
