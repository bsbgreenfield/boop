#![allow(unused)]
use core::panic;
use std::{collections::hash_map, mem};

use crate::{
    parser::{self, Parser, Token},
    value::{ValData, Value},
};

enum Operations {
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    NoOp,
}
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
enum Precedence {
    PrecNone,
    PrecAssignment,
    PrecOr,
    PrecAnd,
    PrecEquality,
    PrecComparison,
    PrecTerm,
    PrecFactor,
    PrecUnary,
    PrecCall,
    PrecPrimary,
}

enum Instruction {
    Operation(Operations),
    ConstantIdx(u8),
}

fn prec_of(operation: &Operations) -> Precedence {
    use Operations::*;
    use Precedence::*;
    match operation {
        OpAdd => PrecTerm,
        OpSubtract => PrecTerm,
        OpMultiply => PrecFactor,
        OpDivide => PrecFactor,
        OpConstant => PrecPrimary,
        NoOp => PrecNone,
        _ => panic!("not yet implemented..."),
    }
}

fn assert_is_constant(maybe_token: Option<Token>) -> Option<Value> {
    if let Some(token) = maybe_token {
        match token {
            Token::TkNum(val) => return Some(val),
            _ => return None,
        };
    } else {
        panic!("Token parsing error");
    }
}

fn assert_is_operator(maybe_token: Option<Token>) -> Token {
    if let Some(token) = maybe_token {
        assert!(match token {
            Token::TkPlus => true,
            Token::TkEquals => true,
            Token::TkMinus => true,
            _ => false,
        });
        return token;
    } else {
        return Token::TkErr;
    };
}

fn make_constant(maybe_val: Option<Value>, compiler: &mut Compiler) -> Result<u8, &'static str> {
    if let Some(value) = maybe_val {
        let idx = compiler.constants.len();
        compiler.constants.push(value);
        return Ok(idx.try_into().unwrap());
    } else {
        return Err("Failed to allocate new constant ");
    }
}

fn token_to_operator(token: Token) -> Operations {
    use Operations::*;
    use Token::*;
    return match token {
        TkPlus => OpAdd,
        TkMinus => OpSubtract,
        _ => panic!("not a valid operator token"),
    };
}

fn emit_constant(operation: Operations, maybe_val: Option<Value>, compiler: &mut Compiler) {
    if let Ok(idx) = make_constant(maybe_val, compiler) {
        compiler.code.push(Instruction::Operation(operation));
        compiler.code.push(Instruction::ConstantIdx(idx));
    } else {
        panic!("error in alocating constant");
    }
}

fn emit_operation(operation: Operations, compiler: &mut Compiler) {
    compiler.code.push(Instruction::Operation(operation));
}

fn top_of(stack: &Vec<Operations>) -> &Operations {
    let length = stack.len();
    if length == 0 {
        &Operations::NoOp
    } else {
        &stack[length]
    }
}

fn dump_stack(mut stack: Vec<Operations>, compiler: &mut Compiler) -> Vec<Operations> {
    while stack.len() > 0 {
        if let Some(operation) = stack.pop() {
            emit_operation(operation, compiler);
        }
    }
    stack
}

pub struct Compiler<'a> {
    parser: Parser<'a>,
    constants: Vec<Value>,
    code: Vec<Instruction>,
}

impl<'a> Compiler<'a> {
    pub fn new(code: &'a String) -> Self {
        Compiler {
            parser: Parser::new(code),
            constants: Vec::<Value>::new(),
            code: Vec::<Instruction>::new(),
        }
    }

    pub fn expression(&mut self) -> () {
        let mut operator_stack: Vec<Operations> = Vec::with_capacity(8);
        let operand_phase: bool = true;
        loop {
            let maybe_token = self.parser.parse_next();
            if operand_phase {
                // optionally extract the value from the token
                let maybe_val = assert_is_constant(maybe_token);
                emit_constant(Operations::OpConstant, maybe_val, self);
            } else {
                let operator_token: Token = assert_is_operator(maybe_token);
                let operator = token_to_operator(operator_token);
                let top_of_operator_stack: &Operations = top_of(&operator_stack);
                // if the new operator is of a higher precedence than the last, push it onto the
                // stack
                if prec_of(&operator) > prec_of(top_of_operator_stack) {
                    operator_stack.push(operator);
                } else {
                    operator_stack = dump_stack(operator_stack, self);
                }
            }
        }
    }
}
