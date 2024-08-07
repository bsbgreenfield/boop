#![allow(unused)]
use core::panic;

#[derive(PartialEq)]
pub enum ObjType {
    ObjStringType,
    ObjBobjType,
}

pub trait Object {
    fn get_type(&self) -> ObjType {
        match self {
            ObjString => ObjType::ObjStringType,
            ObjBobj => ObjType::ObjBobjType,
        }
    }

    fn get_string(&self) -> &str;
}

pub struct ObjString {
    charData: String,
}

impl Object for ObjString {
    fn get_string(&self) -> &str {
        &self.charData
    }
}

struct ObjBobj {}

impl Object for ObjBobj {
    fn get_string(&self) -> &str {
        panic!("get_string cannot be called on a Object");
    }
}
