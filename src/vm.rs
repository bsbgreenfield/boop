use core::panic;
use std::{path::Iter, rc::Rc, usize};

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
        use Operations::*;
        loop {
            if self.compiler.statement() == false {
                break;
            }
        }
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
                        OpAdd => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num + second_num));
                            println!("OP ADD: {:?}", &self.stack);
                        }
                        OpSubtract => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num - second_num));
                            println!("OP SUBTRACT: {:?}", &self.stack);
                        }
                        OpMultiply => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num * second_num));
                            println!("OP MULTIPLY: {:?}", &self.stack);
                        }
                        OpDivide => {
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num / second_num));
                            println!("OP DIVIDE: {:?}", &self.stack);
                        }
                        OpConcat => {
                            let first_string = self.stack.pop().unwrap();
                            let second_string = self.stack.pop().unwrap();
                            let mut concat = String::from(second_string.unwrap_str());
                            concat.push_str(first_string.unwrap_str());
                            self.stack
                                .push(ValData::ValObj(Rc::new(ObjString::new_from_heap(concat))));
                            println!("OP CONCAT: {:?}", &self.stack);
                            println!("new string is {:?}", self.stack.pop().unwrap().unwrap_str());
                        }
                        OpGetLocal => {
                            if let Some(idx_instr) = instr_iter.next() {
                                match idx_instr {
                                    Instruction::ConstantIdx(idx) => {
                                        let local: ValData = self.stack[*idx as usize].clone();
                                        self.stack.push(local);
                                        println!("OP GET LOCAL: {}", idx);
                                    }
                                    _ => (),
                                }
                            }
                        }
                        OpSetLocal => {
                            let idx_instr = instr_iter.next().unwrap();
                            match idx_instr {
                                Instruction::ConstantIdx(idx) => {
                                    let new_value = self.stack.pop().unwrap();
                                    self.stack[*idx as usize] = new_value;
                                }
                                _ => panic!("expected an idx of the local to set"),
                            }
                        }
                        OpAnd => todo!(),
                        OpOr => todo!(),
                        NoOp => todo!(),
                        OpGrouping => panic!("compiler error..."),
                        OpPop => {
                            self.stack.pop();
                        }
                    },
                    _ => panic!("expected an operation, got {:?}", current_instruction),
                }
            } else {
                break;
            }
        }
        Ok(())
    }
}
