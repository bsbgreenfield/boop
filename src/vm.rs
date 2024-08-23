use core::panic;
use std::{rc::Rc, usize};

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
        debug_instructions(&self.compiler.code);
        let instructions = &mut self.compiler.code;
        let mut instr_iter = instructions.iter();
        let mut curr_instr_idx = 0;
        loop {
            if let Some(current_instruction) = instr_iter.next() {
                match current_instruction {
                    Instruction::Operation(op) => match op {
                        Operations::OpConstant => {
                            let idx =
                                Self::interpret_constant_idx(&mut instr_iter, &mut curr_instr_idx);
                            let val: ValData = self.compiler.constants[idx as usize].data.clone();
                            self.stack.push(val);
                            print!("OP CONSTANT: {:?}  |     ", &idx);
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
                            let idx =
                                Self::interpret_constant_idx(&mut instr_iter, &mut curr_instr_idx);
                            let local: ValData = self.stack[idx as usize].clone();
                            self.stack.push(local);
                            print!("OP GET LOCAL: {} |     ", idx);
                        }
                        OpSetLocal => {
                            let idx_instr = instr_iter.next().unwrap();
                            match idx_instr {
                                Instruction::ConstantIdx(idx) => {
                                    curr_instr_idx += 1;
                                    let new_value = self.stack.pop().unwrap();
                                    self.stack[*idx as usize] = new_value;
                                    print!("OP_SET_LOCAL: {}  |     ", idx);
                                }
                                _ => panic!("expected an idx of the local to set"),
                            }
                        }
                        OpPrint => {
                            self.stack.pop().unwrap().print_value();
                            print!("OP_PRINT        |     ")
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
                            print!("OP_EQUALS       |     ")
                        }
                        NoOp => todo!(),
                        OpGrouping => panic!("compiler error..."),
                        OpPop => {
                            print!("OP_POP          |     ");
                            self.stack.pop();
                        }
                        OpJumpIfFalse => {
                            let idx =
                                Self::interpret_constant_idx(&mut instr_iter, &mut curr_instr_idx);

                            let condition = self.stack.pop().unwrap().unwrap_bool();
                            if !condition {
                                for _ in 0..(idx - curr_instr_idx - 1) {
                                    instr_iter.next();
                                }
                            }
                            print!("OP_JUMP_IF_FALSE: {}|     ", idx);
                        }
                        OpJump => {
                            let idx =
                                Self::interpret_constant_idx(&mut instr_iter, &mut curr_instr_idx);
                        }
                    },
                    _ => panic!("expected an operation, got {:?}", current_instruction),
                }
                println!("{:?}", &self.stack);
            } else {
                break;
            }
            curr_instr_idx += 1;
        }
        Ok(())
    }

    fn interpret_constant_idx<I>(instr_iter: &mut I, curr_instr_idx: &mut u8) -> u8
    where
        I: Iterator<Item = &'a compiler::Instruction>,
    {
        if let Some(idx_instr) = instr_iter.next() {
            *curr_instr_idx += 1;
            match idx_instr {
                Instruction::ConstantIdx(idx) => return *idx,
                _ => panic!("expected a constant idx, got {:?}", idx_instr),
            }
        } else {
            panic!("expected a constant idx");
        }
    }
}

fn debug_instructions(instructions: &Vec<Instruction>) {
    println!("_______________________________");
    for (idx, instruction) in instructions.iter().enumerate() {
        println!("{}: {:?}", idx, instruction);
    }
    println!("_______________________________");
}

fn debug_print_vm(current_instruction: Instruction, stack: &Vec<ValData>) {
    match current_instruction {
        Instruction::Operation(op) => print!("{:?}", op),
        Instruction::ConstantIdx(idx) => print!(": {:?}", idx),
    }
    print!(" |     ");
    println!("{:?}", stack);
}
