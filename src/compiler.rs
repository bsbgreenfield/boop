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
    OpAnd,
    OpOr,
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
        OpAnd => PrecAnd,
        OpOr => PrecOr,
        NoOp => PrecNone,
        OpGrouping => PrecNone,
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
        TkAnd => OpAnd,
        TkOr => OpOr,
        TkFalse | TkTrue | TkEof | TkErr | TkFor | TkSemicolon | TkNum | TkEquals
        | TkCloseParen => {
            panic!("Expected an operator token, got {:?}", token);
        }
    };
}

fn make_constant(val: Value, compiler: &mut Compiler) -> Result<u8, &'static str> {
    let idx = compiler.constants.len();
    compiler.constants.push(val);
    return Ok(idx.try_into().unwrap());
}

fn emit_constant(val: Value, compiler: &mut Compiler) {
    match val.val_type {
        ValType::ValNumType | ValType::ValBoolType => {
            if let Ok(idx) = make_constant(val, compiler) {
                compiler
                    .code
                    .push(Instruction::Operation(Operations::OpConstant));
                compiler.code.push(Instruction::ConstantIdx(idx));
            } else {
                panic!("error in alocating constant");
            }
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

fn can_add_or_subtract(
    operand_1: ValType,
    operand_2: ValType,
    operand_type_stack: &mut Vec<ValType>,
) -> bool {
    if operand_1 == ValType::ValNumType && operand_2 == ValType::ValNumType {
        push_type(ValType::ValNumType, operand_type_stack);
        return true;
    } // TODO: else if valstring and valstring return true, and OpConcat
    return false;
}

fn can_multiply_or_divide(
    operand_1: ValType,
    operand_2: ValType,
    operand_type_stack: &mut Vec<ValType>,
) -> bool {
    if operand_1 == ValType::ValNumType && operand_2 == ValType::ValNumType {
        push_type(ValType::ValNumType, operand_type_stack);
        return true;
    }
    return false;
}

fn can_and_or_or(
    operand_1: ValType,
    operand_2: ValType,
    operand_type_stack: &mut Vec<ValType>,
) -> bool {
    if operand_1 == ValType::ValBoolType && operand_2 == ValType::ValBoolType {
        push_type(ValType::ValBoolType, operand_type_stack);
        return true;
    }
    return false;
}

fn dump_stack(
    stack: &mut Vec<Operations>,
    operand_type_stack: &mut Vec<ValType>,
    compiler: &mut Compiler,
) -> () {
    println!("type_stack: {:?}", operand_type_stack);
    println!("operator stack: {:?}", stack);
    // dump until we hit a grouing operation, then pop that and exit
    while stack.len() > 0 {
        if let Some(operation) = stack.pop() {
            match operation {
                Operations::OpGrouping => {
                    break;
                }
                _ => {
                    let operand_1 = operand_type_stack.pop().unwrap();
                    let operand_2 = operand_type_stack.pop().unwrap();
                    match operation {
                        Operations::OpAdd | Operations::OpSubtract => {
                            if can_add_or_subtract(operand_1, operand_2, operand_type_stack) {
                                emit_operation(operation, compiler);
                            } else {
                                panic!(
                                    "Type mismatch: can't add or subtract {:?} to {:?}",
                                    operand_1, operand_2
                                );
                            }
                        }
                        Operations::OpMultiply | Operations::OpDivide => {
                            if can_multiply_or_divide(operand_1, operand_2, operand_type_stack) {
                                emit_operation(operation, compiler);
                            } else {
                                panic!(
                                    "Type mismatch: cant multipply or divide {:?}, with {:?}",
                                    operand_1, operand_2
                                );
                            }
                        }
                        Operations::OpAnd | Operations::OpOr => {
                            if can_and_or_or(operand_1, operand_2, operand_type_stack) {
                                emit_operation(operation, compiler);
                            } else {
                                panic!("Type mismatch: can only use 'and' and 'or' operators with booleans, found {:?}, and {:?}", operand_1, operand_2);
                            }
                        }

                        _ => (),
                    }
                }
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

fn push_type_of_val(val: &Value, operand_type_stack: &mut Vec<ValType>) {
    operand_type_stack.push(val.val_type);
}

fn push_type(val_type: ValType, operand_type_stack: &mut Vec<ValType>) {
    operand_type_stack.push(val_type);
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
            TkPlus | TkMinus | TkStar | TkSlash | TkAnd | TkOr => true,
            _ => false,
        });
        return token;
    }

    pub fn expression(&mut self) -> () {
        let mut operator_stack: Vec<Operations> = Vec::new();
        let mut operand_type_stack: Vec<ValType> = Vec::new();
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
                        dump_stack(&mut operator_stack, &mut operand_type_stack, self);
                        continue;
                    }
                    // compile constant
                    if operand_phase {
                        let val = Self::assert_is_constant(self, token);
                        push_type_of_val(&val, &mut operand_type_stack);
                        emit_constant(val, self);
                        operand_phase = false;
                    }
                    // compile operator
                    else {
                        let operator = token_to_operator(&Self::assert_is_operator(self, token));
                        let top_of_operator_stack: &Operations = top_of(&operator_stack);
                        // if the new operator is of a higher precedence than the last, push it onto the
                        // stack
                        if prec_of(&operator) > prec_of(top_of_operator_stack) {
                            operator_stack.push(operator);
                        } else {
                            dump_stack(&mut operator_stack, &mut operand_type_stack, self);
                            operator_stack.push(operator);
                        }
                        operand_phase = true;
                    }
                }
                None => {
                    dump_stack(&mut operator_stack, &mut operand_type_stack, self);
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
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ],
        );

        assert_eq!(&compiler.constants, &vec![Value::from_bool(true)])
    }

    #[test]
    fn compile_a_boolean_operation() {
        let instruction_buffer: Vec<Instruction>;
        let code: &String = &String::from("true and (false or false) and true");
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
                Instruction::from_operation(OpOr),
                Instruction::from_operation(OpAnd),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(3),
                Instruction::from_operation(OpAnd),
            ],
        );

        assert_eq!(
            &compiler.constants,
            &vec![
                Value::from_bool(true),
                Value::from_bool(false),
                Value::from_bool(false),
                Value::from_bool(true)
            ]
        );
    }

    #[test]
    #[should_panic]
    fn panic_adding_num_and_bool() {
        let instruction_buffer: Vec<Instruction>;
        let code: &String = &String::from("1 + (true and true)");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression();
    }

    #[test]
    #[should_panic]
    fn panic_multiplying_num_and_bool() {
        let instruction_buffer: Vec<Instruction>;
        let code: &String = &String::from("1 * false + 2");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression();
    }
}
