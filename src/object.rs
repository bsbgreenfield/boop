#![allow(unused)]
use core::panic;
use crate::compiler::*;
use crate::{
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

    fn get_parameters(&self) -> &[ValType] {
        panic!("can only get parameters from an objFunction");
    }

    fn get_return_type(&self) -> ValType {
        panic!("can only get the return type of an objFunction");
    }

    fn get_chunk(&self) -> &Chunk {
        panic!("can only get the chunk associated wtih an objfunction");
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
    pub return_type: ValType
}

impl Object for ObjFunction {

    fn get_string(&self) -> &str {
        "ObjFunction"
    } 
    fn get_parameters(&self) -> &[ValType] {
        self.parameters.as_slice()
    }

    fn get_return_type(&self) -> ValType {
        self.return_type
    }

    fn get_chunk(&self) -> &Chunk {
        &self.chunk
    }
    
}

impl ObjFunction {
    pub fn new(f_type: FunctionType, return_type: ValType) -> Self {
        ObjFunction {
            f_type,
            chunk: Chunk::new(),
            parameters : Vec::<ValType>::new(),
            return_type
        }
    }

    pub fn set_params(&mut self, parameters: Vec<ValType>){
        self.parameters = parameters;
    }
}
