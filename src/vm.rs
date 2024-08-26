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
        let mut breakpoints: Vec<(usize, usize)> = Vec::new();
        let mut idx = 0;
        loop {
            if idx > instructions.len() - 1 {
                println!("FINISHED INTERPRETING!");
                break;
            }
            let current_instruction = &instructions[idx];
            match current_instruction {
                Instruction::Operation(op) => match op {
                    Operations::OpConstant => {
                        let constant_idx = get_constant_idx(&mut idx, instructions);
                        let val: ValData =
                            self.compiler.constants[constant_idx as usize].data.clone();
                        self.stack.push(val);
                        print!("OP CONSTANT: {:?}  |     ", &constant_idx);
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
                        let constant_idx = get_constant_idx(&mut idx, instructions);
                        let local: ValData = self.stack[constant_idx as usize].clone();
                        self.stack.push(local);
                        print!("OP GET LOCAL: {} |     ", constant_idx);
                    }
                    OpSetLocal => {
                        let constant_idx = get_constant_idx(&mut idx, instructions);
                        let new_value = self.stack.pop().unwrap();
                        self.stack[constant_idx as usize] = new_value;
                        print!("OP_SET_LOCAL: {}  |     ", constant_idx);
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
                        let instruction_idx = get_instruction_idx(&mut idx, instructions);
                        let condition = self.stack.pop().unwrap().unwrap_bool();
                        if !condition {
                            idx = instruction_idx - 1;
                        }
                        print!("OP_JUMP_IF_FALSE: {}|     ", instruction_idx);
                    }
                    OpJump => {
                        let instruction_idx = get_instruction_idx(&mut idx, instructions);
                        idx = instruction_idx;
                    }
                    OpLoop => {
                        let instruction_idx = get_instruction_idx(&mut idx, instructions);
                        breakpoints.push((idx, instruction_idx - 1));
                        print!("OP_LOOP: {}    |     ", instruction_idx);
                    }
                    OpBreak => {
                        if let Some(loop_breakpoint) = breakpoints.pop() {
                            let new_idx = loop_breakpoint.1;
                            print!("OP_BREAK     |    ");
                            idx = new_idx;
                        } else {
                            panic!("break called outside of a loop");
                        }
                    }
                    OpContinue => {
                        if let Some(loop_breakpoint) = &breakpoints.last() {
                            let loop_start = loop_breakpoint.0;
                            print!("OP_CONTINUE   |     ");
                            idx = loop_start;
                        } else {
                            panic!("continue called outside of loop");
                        }
                    }
                },
                _ => panic!("expected an operation, got {:?}", current_instruction),
            }
            idx += 1;
            println!("{:?}", self.stack);
        }
        Ok(())
    }
}

fn get_constant_idx(idx: &mut usize, instructions: &Vec<Instruction>) -> u8 {
    *idx += 1;
    match instructions[*idx] {
        Instruction::ConstantIdx(constant_idx) => constant_idx,
        _ => panic!("expected a constant idx"),
    }
}

fn get_instruction_idx(idx: &mut usize, instructions: &Vec<Instruction>) -> usize {
    *idx += 1;
    match instructions[*idx] {
        Instruction::InstructionIdx(idx) => idx,
        _ => panic!("expected an instruction idx"),
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
        Instruction::InstructionIdx(idx) => print!(": {:?}", idx),
    }
    print!(" |     ");
    println!("{:?}", stack);
}
