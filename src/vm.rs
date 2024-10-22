use core::panic;
use std::{rc::Rc, usize};

use crate::{
    compiler::{Compiler, Instruction, Operations},
    object::ObjString,
    value::{ValData, Value},
};

pub enum RuntimeError {
    BoopError,
}

pub enum InterpretResult {
    Breaked,
    Done,
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
            compiler: Compiler::new(code),
            stack: Vec::with_capacity(256),
        }
    }

    pub fn interpret(
        instructions: &[Instruction],
        constants: &Vec<Value>,
        stack: &mut Vec<ValData>,
        offset: usize,
    ) -> InterpretResult {
        let mut breakpoints: Vec<(usize, usize)> = Vec::new();
        let mut idx = 0;
        loop {
            if idx > instructions.len() - 1 {
                return InterpretResult::Done;
            }
            let current_instruction = &instructions[idx];
            use Operations::*;

            debug_vm(&instructions, idx, &stack);
            match current_instruction {
                Instruction::Operation(op) => match op {
                    Operations::OpConstant => {
                        let constant_idx = get_constant_idx(&mut idx, instructions);
                        let val: ValData = constants[constant_idx as usize].data.clone();
                        stack.push(val);
                    }
                    OpAdd => {
                        let second_num = stack.pop().unwrap().unwrap_int();
                        let first_num = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValNum(first_num + second_num));
                    }
                    OpSubtract => {
                        let second_num = stack.pop().unwrap().unwrap_int();
                        let first_num = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValNum(first_num - second_num));
                    }
                    OpMultiply => {
                        let second_num = stack.pop().unwrap().unwrap_int();
                        let first_num = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValNum(first_num * second_num));
                    }
                    OpDivide => {
                        let second_num = stack.pop().unwrap().unwrap_int();
                        let first_num = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValNum(first_num / second_num));
                    }
                    OpConcat => {
                        let first_string = stack.pop().unwrap();
                        let second_string = stack.pop().unwrap();
                        let mut concat = String::from(second_string.unwrap_str());
                        concat.push_str(first_string.unwrap_str());
                        stack.push(ValData::ValObj(Rc::new(ObjString::new_from_heap(concat))));
                    }
                    OpGetLocal => {
                        let constant_idx = get_constant_idx(&mut idx, instructions);
                        let local: ValData = stack[constant_idx as usize].clone();
                        stack.push(local);
                    }
                    OpSetLocal => {
                        let constant_idx = get_constant_idx(&mut idx, instructions);
                        let new_value = stack.pop().unwrap();
                        stack[constant_idx as usize] = new_value;
                    }
                    OpPrint => {
                        stack.pop().unwrap().print_value();
                    }
                    OpAnd => {
                        let val_2 = stack.pop().unwrap().unwrap_bool();
                        let val_1 = stack.pop().unwrap().unwrap_bool();
                        stack.push(ValData::ValBool(val_1 && val_2));
                    }
                    OpOr => {
                        let val_2 = stack.pop().unwrap().unwrap_bool();
                        let val_1 = stack.pop().unwrap().unwrap_bool();
                        stack.push(ValData::ValBool(val_1 || val_2));
                    }
                    OpLessThan => {
                        let val_2 = stack.pop().unwrap().unwrap_int();
                        let val_1 = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValBool(val_1 < val_2));
                    }
                    OpGreaterThan => {
                        let val_2 = stack.pop().unwrap().unwrap_int();
                        let val_1 = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValBool(val_1 > val_2));
                    }
                    OpGreaterEquals => {
                        let val_2 = stack.pop().unwrap().unwrap_int();
                        let val_1 = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValBool(val_1 >= val_2));
                    }
                    OpLessEquals => {
                        let val_2 = stack.pop().unwrap().unwrap_int();
                        let val_1 = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValBool(val_1 <= val_2));
                    }
                    OpNot => {
                        let val = stack.pop().unwrap().unwrap_bool();
                        stack.push(ValData::ValBool(!val));
                    }
                    OpNotEquals => {
                        let val_2 = stack.pop().unwrap().unwrap_int();
                        let val_1 = stack.pop().unwrap().unwrap_int();
                        stack.push(ValData::ValBool(val_1 != val_2));
                    }
                    OpEquals => {
                        let val_2 = stack.pop().unwrap();
                        let val_1 = stack.pop().unwrap();
                        let does_equal = val_1.compare_value(&val_2);
                        stack.push(ValData::ValBool(does_equal));
                    }
                    NoOp => todo!(),
                    OpGrouping => panic!("compiler error..."),
                    OpPop => {
                        stack.pop();
                    }
                    OpJumpIfFalse => {
                        let instruction_idx = get_instruction_idx(&mut idx, instructions, offset);
                        let condition = stack.pop().unwrap().unwrap_bool();
                        if !condition {
                            idx = instruction_idx - 1;
                        }
                    }
                    OpJump => {
                        let instruction_idx = get_instruction_idx(&mut idx, instructions, offset);
                        idx = instruction_idx;
                    }
                    OpLoop => {
                        let instruction_idx = get_instruction_idx(&mut idx, instructions, offset);
                        breakpoints.push((idx, instruction_idx - 1));
                    }
                    OpBreak => {
                        if let Some(loop_breakpoint) = breakpoints.pop() {
                            let new_idx = loop_breakpoint.1;
                            if new_idx + 1 > instructions.len() - 1 {
                                return InterpretResult::Breaked;
                            }
                            idx = new_idx;
                        } else {
                            panic!("break called outside of a loop");
                        }
                    }
                    OpContinue => {
                        if let Some(loop_breakpoint) = &breakpoints.last() {
                            let loop_start = loop_breakpoint.0;
                            idx = loop_start;
                        } else {
                            panic!("continue called outside of loop");
                        }
                    }
                    OpLoopFor => {
                        let loop_count = stack.pop().unwrap().unwrap_int();
                        idx += 1; // 'OpLoop'
                        let instruction_idx = get_instruction_idx(&mut idx, instructions, offset);
                        for _ in 0..loop_count {
                            match Self::interpret(
                                &instructions[(idx - 1)..(instruction_idx)],
                                constants,
                                stack,
                                idx - 1,
                            ) {
                                InterpretResult::Done => (),
                                InterpretResult::Breaked => break,
                            }
                        }
                        idx = instruction_idx - 1;
                    }
                },
                _ => panic!("expected an operation, got {:?}", current_instruction),
            }
            idx += 1;
        }
    }

    pub fn run(&'a mut self) -> Result<(), RuntimeError> {
        self.compiler.compile();
        debug_instructions(&self.compiler.code);
        let instructions = &self.compiler.code;
        Self::interpret(instructions, &self.compiler.constants, &mut self.stack, 0);
        Ok(())
    }
}

fn get_constant_idx(idx: &mut usize, instructions: &[Instruction]) -> u8 {
    *idx += 1;
    match instructions[*idx] {
        Instruction::ConstantIdx(constant_idx) => constant_idx,
        _ => panic!("expected a constant idx"),
    }
}

fn get_instruction_idx(idx: &mut usize, instructions: &[Instruction], offset: usize) -> usize {
    *idx += 1;
    match instructions[*idx] {
        Instruction::InstructionIdx(idx) => idx - offset,
        _ => panic!("expected an instruction idx, got {:?}", instructions[*idx]),
    }
}

fn debug_instructions(instructions: &Vec<Instruction>) {
    println!("_______________________________");
    for (idx, instruction) in instructions.iter().enumerate() {
        println!("{}: {:?}", idx, instruction);
    }
    println!("_______________________________");
}

fn debug_vm(instructions: &[Instruction], instr_idx: usize, stack: &Vec<ValData>) {
    use Operations::*;
    let instruction = &instructions[instr_idx];
    let next_instruction;
    if instr_idx > instructions.len() - 2 {
        next_instruction = None;
    } else {
        next_instruction = Some(&instructions[instr_idx + 1]);
    }
    let idx: u8 = match next_instruction {
        Some(instr) => match instr {
            Instruction::InstructionIdx(idx) => *idx as u8,
            Instruction::ConstantIdx(idx) => *idx,
            _ => 0,
        },
        None => 0,
    };
    match instruction {
        Instruction::Operation(op) => match op {
            OpConstant => {
                print!("OP CONSTANT: {:?}      |     ", idx);
            }
            OpAdd => {
                print!("OP ADD              |     ");
            }
            OpSubtract => {
                print!("OP SUBTRACT          |     ");
            }
            OpMultiply => {
                print!("OP MULTIPLY         |     ");
            }
            OpDivide => {
                print!("OP DIVIDE           |     ");
            }
            OpConcat => {
                print!("OP CONCAT          |     ");
            }
            OpGetLocal => {
                print!("OP GET LOCAL: {}     |     ", idx);
            }
            OpSetLocal => {
                print!("OP_SET_LOCAL: {}     |     ", idx);
            }
            OpPrint => {
                print!("OP_PRINT            |     ");
            }
            OpAnd => {
                print!("OP_AND           |     ");
            }
            OpOr => {
                print!("OP_OR            |     ");
            }
            OpLessThan => {
                print!("OP_LESS              |     ");
            }
            OpGreaterThan => {
                print!("OP_GREATER          |     ");
            }
            OpEquals => {
                print!("OP_EQUALS           |     ");
            }
            OpLessEquals => {
                print!("OP_LESS_EQUALS      |     ");
            }
            OpGreaterEquals => {
                print!("OP_GREATER_EQUALS   |     ");
            }
            OpNotEquals => {
                print!("OP_EQUALS            |     ");
            }
            OpNot => {
                print!("OP_NOT               |     ");
            }
            NoOp => todo!(),
            OpGrouping => panic!("compiler error..."),
            OpPop => {
                print!("OP_POP              |     ");
            }
            OpJumpIfFalse => {
                print!("OP_JUMP_IF_FALSE: {}|     ", idx);
            }
            OpJump => {
                print!("OP_JUMP {}           |     ", idx);
            }
            OpLoop => {
                print!("OP_LOOP: {}         |     ", idx);
            }
            OpBreak => {
                print!("OP_BREAK            |    ");
            }
            OpContinue => {
                print!("OP_CONTINUE       |     ");
            }
            OpLoopFor => {
                print!(
                    "OP_LOOP_FOR {:?}       |     ",
                    stack.last().unwrap().unwrap_int()
                );
            }
        },
        _ => (),
    }
    println!("{:?}", stack);
}
