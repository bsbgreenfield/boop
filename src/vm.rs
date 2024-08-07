use core::panic;
use std::{path::Iter, rc::Rc};

use crate::{
    compiler::{Compiler, Instruction, Operations},
    object::ObjString,
    value::{ValData, Value},
};

pub enum RuntimeError {
    BoopError,
}

pub struct Vm<'a> {
    code_text: &'a String,
    compiler: Compiler<'a>,
    stack: Vec<ValData>,
}

impl<'a> Vm<'a> {
    pub fn new(code: &'a String) -> Self {
        Self {
            code_text: code,
            compiler: Compiler::new(&code),
            stack: Vec::with_capacity(256),
        }
    }

    pub fn run(&'a mut self) -> Result<(), RuntimeError> {
        self.compiler.expression();
        let instructions = &mut self.compiler.code;
        let mut instr_iter = instructions.iter();
        loop {
            if let Some(current_instruction) = instr_iter.next() {
                match current_instruction {
                    Instruction::Operation(op) => match op {
                        Operations::OpConstant => {
                            if let Some(idx_instr) = instr_iter.next() {
                                match idx_instr {
                                    Instruction::ConstantIdx(idx) => {
                                        let val: ValData =
                                            self.compiler.constants[*idx as usize].data.clone();
                                        self.stack.push(val);
                                        println!("OP CONSTANT: {:?}", &self.stack);
                                    }
                                    _ => panic!("expected an idx"),
                                }
                            }
                        }
                        Operations::OpAdd => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num + second_num));
                            println!("OP ADD: {:?}", &self.stack);
                        }
                        Operations::OpSubtract => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num - second_num));
                            println!("OP SUBTRACT: {:?}", &self.stack);
                        }
                        Operations::OpMultiply => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num * second_num));
                            println!("OP MULTIPLY: {:?}", &self.stack);
                        }
                        Operations::OpDivide => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num / second_num));
                            println!("OP DIVIDE: {:?}", &self.stack);
                        }
                        Operations::OpConcat => {
                            let first_string = self.stack.pop().unwrap();
                            let second_string = self.stack.pop().unwrap();
                            let mut concat = String::from(second_string.unwrap_str());
                            concat.push_str(first_string.unwrap_str());
                            self.stack
                                .push(ValData::ValObj(Rc::new(ObjString::new_from_heap(concat))));
                            println!("OP DIVIDE: {:?}", &self.stack);
                            println!("new string is {:?}", self.stack.pop().unwrap().unwrap_str());
                        }
                        _ => panic!("not implemented"),
                    },
                    _ => panic!("expexted an operation, got {:?}", current_instruction),
                }
            } else {
                break;
            }
        }
        Ok(())
    }
}
