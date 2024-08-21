use core::panic;
use std::{path::Iter, rc::Rc, usize};

use crate::{
    compiler::{self, Compiler, Instruction, Operations},
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
        self.compiler.compile();
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
                                        print!("OP CONSTANT: {:?}  |     ", &idx);
                                    }
                                    _ => panic!("expected an idx"),
                                }
                            }
                        }
                        OpAdd => {
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num + second_num));
                            print!("OP ADD          |     ");
                        }
                        OpSubtract => {
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num - second_num));
                            print!("OP SUBTRACT      |     ");
                        }
                        OpMultiply => {
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num * second_num));
                            print!("OP MULTIPLY     |     ");
                        }
                        OpDivide => {
                            let second_num = self.stack.pop().unwrap().unwrap_int();
                            let first_num = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValNum(first_num / second_num));
                            print!("OP DIVIDE       |     ");
                        }
                        OpConcat => {
                            let first_string = self.stack.pop().unwrap();
                            let second_string = self.stack.pop().unwrap();
                            let mut concat = String::from(second_string.unwrap_str());
                            concat.push_str(first_string.unwrap_str());
                            self.stack
                                .push(ValData::ValObj(Rc::new(ObjString::new_from_heap(concat))));
                            print!("OP CONCAT      |     ");
                        }
                        OpGetLocal => {
                            if let Some(idx_instr) = instr_iter.next() {
                                match idx_instr {
                                    Instruction::ConstantIdx(idx) => {
                                        let local: ValData = self.stack[*idx as usize].clone();
                                        self.stack.push(local);
                                        print!("OP GET LOCAL: {} |     ", idx);
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
                                    println!("OP_SET_LOCAL: {}  |     ", idx);
                                }
                                _ => panic!("expected an idx of the local to set"),
                            }
                        }
                        OpPrint => {
                            self.stack.pop().unwrap().print_value();
                        }
                        OpAnd => todo!(),
                        OpOr => todo!(),
                        OpLessThan => {
                            let val_2 = self.stack.pop().unwrap().unwrap_int();
                            let val_1 = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValBool(val_1 < val_2));
                        }
                        OpGreaterThan => {
                            let val_2 = self.stack.pop().unwrap().unwrap_int();
                            let val_1 = self.stack.pop().unwrap().unwrap_int();
                            self.stack.push(ValData::ValBool(val_1 > val_2));
                        }
                        OpEquals => {
                            let val_2 = self.stack.pop().unwrap();
                            let val_1 = self.stack.pop().unwrap();
                            let does_equal = val_1.compare_value(&val_2);
                            self.stack.push(ValData::ValBool(does_equal));
                        }
                        NoOp => todo!(),
                        OpGrouping => panic!("compiler error..."),
                        OpPop => {
                            print!("OP_POP          |     ");
                            self.stack.pop();
                        }
                    },
                    _ => panic!("expected an operation, got {:?}", current_instruction),
                }
                println!("{:?}", &self.stack);
            } else {
                break;
            }
        }
        Ok(())
    }
}

fn debug_print_vm(current_instruction: Instruction, stack: &Vec<ValData>) {
    match current_instruction {
        Instruction::Operation(op) => print!("{:?}", op),
        Instruction::ConstantIdx(idx) => print!(": {:?}", idx),
    }
    print!(" |     ");
    println!("{:?}", stack);
}
