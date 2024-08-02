#![allow(unused)]
use core::fmt;
use core::panic;
use std::{collections::hash_map, mem};

use crate::value;
use crate::value::ValType;
use crate::{
    parser::{self, Parser, Token},
    value::{ValData, Value},
};

#[derive(Debug, PartialEq)]
enum Operations {
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    NoOp,
    OpGrouping,
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
    PrecGrouping,
}

#[derive(PartialEq)]
enum Instruction {
    Operation(Operations),
    ConstantIdx(u8),
}

impl Instruction {
    pub fn from_operation(op: Operations) -> Self {
        Instruction::Operation(op)
    }

    pub fn from_constant_idx(idx: u8) -> Self {
        Instruction::ConstantIdx(idx)
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Operation(op) => write!(f, "Operation({:?})", op),
            Instruction::ConstantIdx(idx) => write!(f, "ConstantIdx({})", idx),
        }
    }
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
        OpGrouping => PrecNone,
        _ => panic!("not yet implemented..."),
    }
}

fn token_to_operator(token: &Token) -> Operations {
    use Operations::*;
    use Token::*;
    return match token {
        TkPlus => OpAdd,
        TkMinus => OpSubtract,
        TkStar => OpMultiply,
        TkSlash => OpDivide,
        TkOpenParen => OpGrouping,
        _ => panic!("not a valid operator token"),
    };
}

fn make_constant(val: Value, compiler: &mut Compiler) -> Result<u8, &'static str> {
    let idx = compiler.constants.len();
    compiler.constants.push(val);
    return Ok(idx.try_into().unwrap());
}

fn emit_constant(val: Value, compiler: &mut Compiler) {
    match val.val_type {
        ValType::ValNumType => {
            if let Ok(idx) = make_constant(val, compiler) {
                compiler
                    .code
                    .push(Instruction::Operation(Operations::OpConstant));
                compiler.code.push(Instruction::ConstantIdx(idx));
            } else {
                panic!("error in alocating constant");
            }
        }
        ValType::ValBoolType => {
            compiler.constants.push(val);
            compiler
                .code
                .push(Instruction::Operation(Operations::OpConstant));
        }
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
        &stack[length - 1]
    }
}

fn is_group_start(token: &Token) -> bool {
    match token {
        Token::TkOpenParen => true,
        _ => false,
    }
}

fn is_group_end(token: &Token) -> bool {
    match token {
        Token::TkCloseParen => true,
        _ => false,
    }
}

fn dump_stack(stack: &mut Vec<Operations>, compiler: &mut Compiler) -> () {
    // dump until we hit a grouing operation, then pop that and exit
    while stack.len() > 0 {
        if let Some(operation) = stack.pop() {
            match operation {
                Operations::OpGrouping => {
                    break;
                }
                _ => emit_operation(operation, compiler),
            }
        }
    }
}

fn debug_print_expression(compiler: &Compiler) {
    for instruction in &compiler.code {
        match instruction {
            Instruction::Operation(op) => match op {
                Operations::OpConstant => print!("{:?}: ", op),
                _ => println!("{:?}", op),
            },
            Instruction::ConstantIdx(idx) => println!("{:?}", idx),
        };
    }
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
    fn assert_is_constant(&self, token: Token) -> Value {
        match token {
            Token::TkNum => {
                if let Some(value) =
                    value::val_from_slice(ValType::ValNumType, self.parser.get_curr_slice())
                {
                    return value;
                } else {
                    panic!("problem parsing value");
                }
            }
            Token::TkTrue => {
                return Value::from_bool(true);
            }
            Token::TkFalse => {
                return Value::from_bool(false);
            }
            _ => panic!(
                "Expected a valid constant, received {:?} as a token instead",
                token
            ),
        }
    }

    fn assert_is_operator(&self, token: Token) -> Token {
        use Token::*;
        assert!(match token {
            TkPlus | TkMinus | TkStar | TkSlash => true,
            _ => false,
        });
        return token;
    }

    pub fn expression(&mut self) -> () {
        let mut operator_stack: Vec<Operations> = Vec::with_capacity(8);
        let mut operand_phase: bool = true;
        loop {
            let maybe_token = self.parser.parse_next();
            match maybe_token {
                Some(token) => {
                    // check if the token is a grouping
                    if is_group_start(&token) {
                        operator_stack.push(token_to_operator(&token));
                        continue;
                    } else if is_group_end(&token) {
                        dump_stack(&mut operator_stack, self);
                        continue;
                    }
                    if operand_phase {
                        let val = Self::assert_is_constant(self, token);
                        emit_constant(val, self);
                        operand_phase = false;
                    } else {
                        let operator_token: Token = Self::assert_is_operator(self, token);
                        let operator = token_to_operator(&operator_token);
                        let top_of_operator_stack: &Operations = top_of(&operator_stack);
                        // if the new operator is of a higher precedence than the last, push it onto the
                        // stack
                        if prec_of(&operator) > prec_of(top_of_operator_stack) {
                            operator_stack.push(operator);
                        } else {
                            dump_stack(&mut operator_stack, self);
                            operator_stack.push(operator);
                        }
                        operand_phase = true;
                    }
                }
                None => {
                    dump_stack(&mut operator_stack, self);
                    debug_print_expression(&self);
                    break;
                }
            }
        }
    }
}

mod tests {
    use super::*;
    use Operations::*;

    #[test]
    fn compile_a_single_number() {
        let instruction_buffer: Vec<Instruction>;
        let code: &String = &String::from("123");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ]
        );

        assert_eq!(&compiler.constants, &vec![Value::from_num(123)]);
    }

    #[test]
    fn compile_an_arithmatic_expression() {
        let instruction_buffer: Vec<Instruction>;
        let code: &String = &String::from("1 + (2 * 3)");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpMultiply),
                Instruction::from_operation(OpAdd),
            ]
        );

        assert_eq!(
            &compiler.constants,
            &vec![Value::from_num(1), Value::from_num(2), Value::from_num(3)]
        );
    }
    #[test]
    fn compile_a_boolean() {
        let instruction_buffer: Vec<Instruction>;
        let code: &String = &String::from("true");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression();
        assert_eq!(
            &compiler.code,
            &vec![Instruction::from_operation(OpConstant)],
        );

        assert_eq!(&compiler.constants, &vec![Value::from_bool(true)])
    }
}
