use core::fmt;
use core::panic;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::{collections::hash_map, mem};

use crate::object::ObjString;
use crate::r#type::DataType;
use crate::value;
use crate::value::ValType;
use crate::{
    parser::{self, Parser, Token},
    value::{ValData, Value},
};

#[derive(Debug, PartialEq)]
pub enum Operations {
    OpConstant,
    OpAdd,
    OpConcat,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpAnd,
    OpOr,
    OpPop,
    NoOp,
    OpGrouping,
    OpGetLocal,
    OpSetLocal,
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
pub enum Instruction {
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
        OpConcat => PrecTerm,
        OpSubtract => PrecTerm,
        OpMultiply => PrecFactor,
        OpDivide => PrecFactor,
        OpConstant => PrecPrimary,
        OpAnd => PrecAnd,
        OpOr => PrecOr,
        OpPop => PrecNone,
        NoOp => PrecNone,
        OpGrouping => PrecNone,
        OpGetLocal => PrecPrimary,
        OpSetLocal => PrecAssignment,
    }
}

fn top_of<T>(stack: &Vec<T>) -> Option<&T> {
    let length = stack.len();
    if length == 0 {
        None
    } else {
        Some(&stack[length - 1])
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
    } else if operand_1 == ValType::ValStringType && operand_2 == ValType::ValStringType {
        push_type(ValType::ValStringType, operand_type_stack);
        return true;
    }
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

fn match_token(maybe_token: Option<Token>, expected: Token) -> bool {
    if let Some(token) = maybe_token {
        return token == expected;
    }
    return false;
}

fn match_val_type(val_ident: &str) -> ValType {
    match val_ident {
        "int" => ValType::ValNumType,
        "String" => ValType::ValStringType,
        "bool" => ValType::ValBoolType,
        _ => panic!("havent implemented this type!!!"),
    }
}

struct Local {
    pub name: String,
    depth: u8,
}

impl Local {
    fn new(name: String, depth: u8) -> Self {
        Local { name, depth }
    }
}

pub struct Compiler<'a> {
    parser: Parser<'a>,
    pub constants: Vec<Value>,
    pub code: Vec<Instruction>,
    types: HashSet<String>,
    locals: Vec<Local>,
    scope_depth: u8,
}

impl<'a> Compiler<'a> {
    pub fn new(code: &'a String) -> Self {
        let mut types = HashSet::new();
        types.insert(String::from("int"));
        types.insert(String::from("bool"));
        types.insert(String::from("String"));
        Compiler {
            parser: Parser::new(code),
            constants: Vec::<Value>::new(),
            code: Vec::<Instruction>::new(),
            types,
            locals: Vec::with_capacity(256),
            scope_depth: 0,
        }
    }

    fn dump_stack(
        &mut self,
        stack: &mut Vec<Operations>,
        operand_type_stack: &mut Vec<ValType>,
    ) -> () {
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
                            Operations::OpAdd | Operations::OpConcat | Operations::OpSubtract => {
                                if can_add_or_subtract(operand_1, operand_2, operand_type_stack) {
                                    self.emit_operation(operation);
                                } else {
                                    panic!(
                                        "Type mismatch: can't add or subtract {:?} to {:?}",
                                        operand_1, operand_2
                                    );
                                }
                            }
                            Operations::OpMultiply | Operations::OpDivide => {
                                if can_multiply_or_divide(operand_1, operand_2, operand_type_stack)
                                {
                                    self.emit_operation(operation);
                                } else {
                                    panic!(
                                        "Type mismatch: cant multipply or divide {:?}, with {:?}",
                                        operand_1, operand_2
                                    );
                                }
                            }
                            Operations::OpAnd | Operations::OpOr => {
                                if can_and_or_or(operand_1, operand_2, operand_type_stack) {
                                    self.emit_operation(operation);
                                } else {
                                    panic!("Type mismatch: can only use 'and' and 'or' operators with booleans, found {:?}, and {:?}", operand_1, operand_2);
                                }
                            }
                            Operations::OpConstant
                            | Operations::OpSetLocal
                            | Operations::OpGetLocal
                            | Operations::NoOp
                            | Operations::OpGrouping
                            | Operations::OpPop => (),
                        }
                    }
                }
            }
        }
    }
    fn make_constant(&mut self, val: Value) -> Result<u8, &'static str> {
        let idx = self.constants.len();
        self.constants.push(val);
        return Ok(idx.try_into().unwrap());
    }

    fn token_to_val(&mut self, token: &Token) -> Value {
        match token {
            Token::TkNum => {
                if let Some(value) =
                    value::val_from_slice(ValType::ValNumType, self.parser.get_curr_slice())
                {
                    value
                } else {
                    panic!("problem parsing value");
                }
            }
            Token::TkTrue => Value::from_bool(true),
            Token::TkFalse => Value::from_bool(false),
            Token::TkString => {
                let str_slice = self.parser.get_curr_slice(); // lexeme from code file
                let trimmed = &str_slice[1..str_slice.len() - 1]; // remove quotes
                let obj_string = ObjString::new(trimmed); // create ObjString
                                                          // TODO: store a strings table, and do interning and stuff
                Value::new(ValType::ValStringType, ValData::ValObj(Rc::new(obj_string)))
            }
            _ => panic!(
                "Expected a valid constant, received {:?} as a token instead",
                token
            ),
        }
    }
    fn get_identifier_from_constants(&self, token: &Token) -> Option<&Value> {
        match token {
            Token::TkIdentifier => {
                if let Some(local_idx) = self.has_variable(self.parser.get_curr_slice()) {
                    return Some(self.constants.get(local_idx).unwrap());
                } else {
                    return None;
                }
            }
            _ => panic!("expected identifier token, got {:?}", token),
        }
    }
    fn emit_constant(&mut self, token: &Token) {
        let val: Value = self.token_to_val(token);

        if let Ok(idx) = self.make_constant(val) {
            self.code
                .push(Instruction::Operation(Operations::OpConstant));
            self.code.push(Instruction::ConstantIdx(idx));
        } else {
            panic!("error in alocating constant");
        }
    }

    fn emit_local(&mut self, idx: usize) {
        self.code
            .push(Instruction::from_operation(Operations::OpGetLocal));
        self.code
            .push(Instruction::from_constant_idx(idx.try_into().unwrap()));
    }

    fn emit_operation(&mut self, operation: Operations) {
        self.code.push(Instruction::Operation(operation));
    }

    fn has_variable(&self, name: &str) -> Option<usize> {
        let mut idx: usize = self.locals.len();
        let local_reverse_iter = self.locals.iter().rev();
        for local in local_reverse_iter {
            if local.name == name {
                return Some(idx - 1);
            }
            idx -= 1;
        }
        None
    }

    fn token_to_operator(&self, token: &Token, operand_type: &ValType) -> Operations {
        use Operations::*;
        use Token::*;
        return match token {
            TkPlus => {
                if operand_type == &ValType::ValStringType {
                    OpConcat
                } else if operand_type == &ValType::ValNumType {
                    OpAdd
                } else {
                    panic!("You may only add two numbers or two strings");
                }
            }
            TkMinus => OpSubtract,
            TkStar => OpMultiply,
            TkSlash => OpDivide,
            TkOpenParen => OpGrouping,
            TkAnd => OpAnd,
            TkOr => OpOr,
            TkFalse | TkTrue | TkEof | TkErr | TkFor | TkSemicolon | TkNum | TkEquals
            | TkCloseParen | TkString | TkIdentifier => {
                panic!("Expected an operator token, got {:?}", token);
            }
        };
    }

    fn assert_is_constant(&self, token: &Token) -> () {
        use Token::*;
        assert!(match token {
            TkNum | TkString | TkTrue | TkFalse | TkIdentifier => true,
            _ => false,
        })
    }

    fn assert_is_operator(&self, token: &Token) -> () {
        use Token::*;
        assert!(match token {
            TkPlus | TkMinus | TkStar | TkSlash | TkAnd | TkOr => true,
            _ => false,
        });
    }

    pub fn statement(&mut self) -> () {
        if let Some(token) = self.parser.peek() {
            match token {
                Token::TkNum
                | Token::TkString
                | Token::TkOpenParen
                | Token::TkTrue
                | Token::TkFalse => {
                    self.expression_statement();
                }
                Token::TkFor => todo!(),
                Token::TkIdentifier => self.variable(),
                Token::TkPlus
                | Token::TkMinus
                | Token::TkStar
                | Token::TkSlash
                | Token::TkErr
                | Token::TkSemicolon
                | Token::TkOr
                | Token::TkAnd
                | Token::TkEquals
                | Token::TkEof
                | Token::TkCloseParen => {
                    panic!("expected a statement or expression");
                }
            }
        }
        if let Some(maybe_semicolon) = self.parser.parse_next() {
            match maybe_semicolon {
                Token::TkSemicolon => (),
                _ => panic!(
                    "expected a semicolon at the end of a statement, found {:?}",
                    maybe_semicolon
                ),
            }
        } else {
            panic!("expected semicolon, found EOF");
        }
    }

    fn compile_operand(&mut self, token: &Token, operand_type_stack: &mut Vec<ValType>) {
        self.assert_is_constant(token);
        // if this is a variable, get local, else emit constant
        if token == &Token::TkIdentifier {
            let ident = self.parser.get_curr_slice();
            let idx = self.has_variable(ident).unwrap();
            self.emit_local(idx);
        } else {
            self.emit_constant(token);
        }
        let new_val_ref = self.constants.last().unwrap();
        push_type_of_val(new_val_ref, operand_type_stack);
    }

    fn variable(&mut self) {
        self.parser.peek();
        if let Some(type_ident) = self.types.get(self.parser.get_curr_slice().trim_end()) {
            self.variable_declaration(match_val_type(type_ident));
        } else {
            self.expression_statement();
        }
    }

    pub fn variable_declaration(&mut self, var_type: ValType) -> () {
        self.parser.parse_next(); // type name
        self.parser.parse_next(); // identifier
        let name = self.parser.get_curr_slice().to_owned();
        if let Some(_) = self.has_variable(&name) {
            panic!(
                "A variable with the name {} already exists in this scope",
                name
            );
        }
        if match_token(self.parser.parse_next(), Token::TkEquals) {
            let return_type = self.expression();
            if var_type != return_type {
                panic!("cannot assign type {:?} to {:?}", var_type, return_type);
            }
            let local = Local::new(String::from(name), self.scope_depth);
            self.locals.push(local);
        }
    }

    pub fn expression_statement(&mut self) {
        self.expression();
        self.emit_operation(Operations::OpPop);
    }

    pub fn expression(&mut self) -> ValType {
        let mut operator_stack: Vec<Operations> = Vec::new();
        let mut operand_type_stack: Vec<ValType> = Vec::new();
        let mut operand_phase: bool = true;

        loop {
            let maybe_token = self.parser.parse_next();
            match maybe_token {
                Some(token) => {
                    // check if the token is a grouping
                    if is_group_start(&token) {
                        if let Some(operator) = top_of(&operand_type_stack) {
                            operator_stack.push(self.token_to_operator(&token, operator));
                        }
                        continue;
                    } else if is_group_end(&token) {
                        self.dump_stack(&mut operator_stack, &mut operand_type_stack);
                        continue;
                    }
                    // compile constant
                    if operand_phase {
                        self.compile_operand(&token, &mut operand_type_stack);
                        operand_phase = false;
                    }
                    // compile operator
                    else {
                        self.assert_is_operator(&token);
                        let operator =
                            self.token_to_operator(&token, top_of(&operand_type_stack).unwrap());
                        let top_of_operator_stack: &Operations =
                            top_of(&operator_stack).unwrap_or(&Operations::NoOp);
                        // if the new operator is of a higher precedence than the last, push it onto the
                        // stack
                        if prec_of(&operator) > prec_of(top_of_operator_stack) {
                            operator_stack.push(operator);
                        } else {
                            self.dump_stack(&mut operator_stack, &mut operand_type_stack);
                            operator_stack.push(operator);
                        }
                        operand_phase = true;
                    }
                    // check for end of expression
                    if let Some(maybe_semicolon) = self.parser.peek() {
                        match maybe_semicolon {
                            Token::TkSemicolon => {
                                self.dump_stack(&mut operator_stack, &mut operand_type_stack);
                                debug_print_expression(&self);
                                break;
                            }
                            _ => (),
                        }
                    }
                }
                None => {
                    self.dump_stack(&mut operator_stack, &mut operand_type_stack);
                    debug_print_expression(&self);
                    break;
                }
            }
        }
        assert_eq!(operand_type_stack.len(), 1);
        return *operand_type_stack.get(0).unwrap();
    }
}

mod tests {
    use super::*;
    use fmt::Write;
    use Operations::*;

    #[test]
    fn compile_a_single_number() {
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
        let code: &String = &String::from("1 + (true and true)");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression();
    }

    #[test]
    #[should_panic]
    fn panic_multiplying_num_and_bool() {
        let code: &String = &String::from("1 * false + 2");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression();
    }

    #[test]
    fn compile_a_string_literal() {
        let mut my_string = String::from('"');
        my_string.push_str("hello");
        my_string.write_char('"').unwrap();
        let mut compiler: Compiler = Compiler::new(&my_string);
        compiler.expression();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ],
        );

        assert_eq!(&compiler.constants, &vec![Value::from_string("hello"),]);
    }

    #[test]
    fn compile_string_concat() {
        let mut my_string_1 = String::from('"');
        my_string_1.push_str("hello");
        my_string_1.write_char('"').unwrap();
        let mut my_string_2 = String::from('"');
        my_string_2.push_str("world");
        my_string_2.write_char('"').unwrap();
        let my_string_3 = String::from(" + ");
        my_string_1.push_str(&my_string_3);
        my_string_1.push_str(&my_string_2);
        let mut compiler: Compiler = Compiler::new(&my_string_1);
        compiler.expression();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpConcat),
            ],
        );

        assert_eq!(
            &compiler.constants,
            &vec![Value::from_string("hello"), Value::from_string(" world")]
        );
    }

    #[test]
    fn compile_a_local_var_dec() {
        let code = String::from("int myNumber = 1;");
        let mut compiler = Compiler::new(&code);
        compiler.statement();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ]
        );

        assert_eq!(&compiler.constants, &vec![Value::from_num(1)]);
    }

    #[test]
    fn compile_multiple_statements() {
        let code = String::from("int myNumber = 1; String myString = \"hello\"; ");
        let mut compiler = Compiler::new(&code);
        compiler.statement();
        compiler.statement();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
            ]
        );

        assert_eq!(
            &compiler.constants,
            &vec![Value::from_num(1), Value::from_string("hello")]
        );
    }

    #[test]
    fn compile_get_local() {
        let code = String::from("int myNumber = 1; myNumber + 2;");
        let mut compiler = Compiler::new(&code);
        compiler.statement();
        compiler.statement();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpAdd),
                Instruction::from_operation(OpPop),
            ]
        );

        assert_eq!(
            &compiler.constants,
            &vec![Value::from_num(1), Value::from_num(2)]
        );
    }
}
