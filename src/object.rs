#![allow(unused)]
use core::panic;

use crate::{
    compiler::{Chunk, Instruction},
    value::{ValType, Value},
};

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

    fn get_string(&self) -> &str {
        panic!("get_string unimplemented for this type of object");
    }
}

pub struct ObjString {
    char_data: String,
}

impl ObjString {
    pub fn new(string: &str) -> Self {
        ObjString {
            char_data: String::from(string),
        }
    }

    pub fn new_from_heap(string: String) -> Self {
        ObjString { char_data: string }
    }
}

impl Object for ObjString {
    fn get_string(&self) -> &str {
        &self.char_data
    }
}

struct ObjBobj {}

impl Object for ObjBobj {
    fn get_string(&self) -> &str {
        panic!("get_string cannot be called on a Object");
    }
}

pub enum FunctionType {
    Script,
    Function,
    Method,
}

pub struct ObjFunction {
    pub f_type: FunctionType,
    pub parameters: Vec<ValType>,
    pub chunk: Chunk,
}

impl Object for ObjFunction {}

impl ObjFunction {
    pub fn new(f_type: FunctionType) -> Self {
        ObjFunction {
            f_type,
            chunk: Chunk::new(),
            parameters : Vec::<ValType>::new(),
        }
    }

    pub fn set_params(&mut self, parameters: Vec<ValType>) -> () {
        self.parameters = parameters;
    }
}
