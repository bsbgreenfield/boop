use core::fmt;
use core::panic;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::usize;
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
    OpGreaterThan,
    OpLessThan,
    OpJump,
    OpEquals,
    OpPop,
    OpLoop,
    OpBreak,
    OpContinue,
    OpJumpIfFalse,
    NoOp,
    OpGrouping,
    OpGetLocal,
    OpSetLocal,
    OpPrint,
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
    InstructionIdx(usize),
}

impl Instruction {
    pub fn from_operation(op: Operations) -> Self {
        Instruction::Operation(op)
    }

    pub fn from_constant_idx(idx: u8) -> Self {
        Instruction::ConstantIdx(idx)
    }
    pub fn from_instruction_idx(idx: usize) -> Self {
        Instruction::InstructionIdx(idx)
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Operation(op) => write!(f, "Operation({:?})", op),
            Instruction::ConstantIdx(idx) => write!(f, "ConstantIdx({})", idx),
            Instruction::InstructionIdx(idx) => write!(f, "InstructionIdx({})", idx),
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
        OpGreaterThan => PrecComparison,
        OpLessThan => PrecComparison,
        OpEquals => PrecEquality,
        OpPop => PrecNone,
        NoOp => PrecNone,
        OpGrouping => PrecNone,
        OpGetLocal => PrecPrimary,
        OpSetLocal => PrecAssignment,
        OpPrint => PrecNone,
        OpJump => PrecNone,
        OpJumpIfFalse => PrecNone,
        OpLoop => PrecNone,
        OpBreak => PrecNone,
        OpContinue => PrecNone,
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

fn can_compare(
    operand_1: ValType,
    operand_2: ValType,
    operand_type_stack: &mut Vec<ValType>,
) -> bool {
    println!("comparing a {:?} to a {:?}", operand_1, operand_2);
    if operand_1 == operand_2 {
        push_type(ValType::ValBoolType, operand_type_stack);
        return true;
    }
    return false;
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

#[derive(Debug)]
struct Local {
    pub name: String,
    pub val_type: ValType,
    depth: u8,
}

impl Local {
    fn new(name: String, depth: u8, val_type: ValType) -> Self {
        Local {
            name,
            depth,
            val_type,
        }
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

    fn token_to_val(&mut self, token: &Token) -> Value {
        match token {
            Token::TkNum => {
                if let Some(value) =
                    value::val_from_slice(ValType::ValNumType, self.parser.get_curr_slice())
                {
                    value
                } else {
                    panic!(
                        "problem parsing value, parsed {:?}",
                        self.parser.get_curr_slice()
                    );
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
            TkLessThan => OpLessThan,
            TkGreaterThan => OpLessThan,
            TkDoubleEquals => OpEquals,
            _ => {
                panic!("Expected an operator token, got {:?}", token);
            }
        };
    }

    fn make_constant(&mut self, val: Value) -> Result<u8, &'static str> {
        let idx = self.constants.len();
        self.constants.push(val);
        return Ok(idx.try_into().unwrap());
    }

    fn emit_jif(&mut self, idx: usize) {
        self.code
            .push(Instruction::from_operation(Operations::OpJumpIfFalse));
        self.code.push(Instruction::from_instruction_idx(idx));
    }

    fn emit_jump(&mut self, idx: usize) {
        self.code
            .push(Instruction::from_operation(Operations::OpJump));
        self.code.push(Instruction::from_instruction_idx(idx));
    }

    fn emit_loop(&mut self, idx: usize) {
        self.code
            .push(Instruction::from_operation(Operations::OpLoop));
        self.code.push(Instruction::from_instruction_idx(idx));
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

    fn emit_get_local(&mut self, idx: usize) {
        self.code
            .push(Instruction::from_operation(Operations::OpGetLocal));
        self.code
            .push(Instruction::from_constant_idx(idx.try_into().unwrap()));
    }

    fn emit_set_local(&mut self, idx: usize) {
        self.code
            .push(Instruction::from_operation(Operations::OpSetLocal));
        self.code
            .push(Instruction::from_constant_idx(idx.try_into().unwrap()));
    }

    fn emit_operation(&mut self, operation: Operations) {
        self.code.push(Instruction::Operation(operation));
    }

    pub fn compile(&mut self) {
        loop {
            if let Some(token) = self.parser.peek() {
                match token {
                    Token::TkOpenBracket => {
                        self.block();
                    }
                    _ => {
                        if !self.statement() {
                            println!("locals!!!!!!!!!!: {:?}", self.locals);
                            return;
                        }
                    }
                };
            } else {
                println!("locals!!!!!!!!!!: {:?}", self.locals);
                return;
            }
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let mut count = self.locals.len();
        for local in self.locals.iter().rev() {
            if local.depth < self.scope_depth {
                break;
            }
            count -= 1;
        }

        for _ in 0..(self.locals.len() - count) {
            // pop all the locals off the stack that no longer exist
            self.emit_operation(Operations::OpPop);
        }
        self.locals.drain(count..); // remove the locals from the locals array
        self.scope_depth -= 1; //return the scope
    }

    pub fn block(&mut self) {
        // consume the open bracket
        if !match_token(self.parser.parse_next(), Token::TkOpenBracket) {
            panic!("Expected '{{' at the beginning of a block");
        }
        self.begin_scope();
        loop {
            if let Some(token) = self.parser.peek() {
                match token {
                    Token::TkCloseBracket => {
                        self.parser.parse_next();
                        self.end_scope();
                        return; // block end, exit function
                    }
                    Token::TkOpenBracket => self.block(),
                    _ => {
                        if self.statement() {
                            continue;
                        }
                        panic!("expected a '}}' to end the block");
                    }
                }
            } else {
                panic!("unexpected EOF before block close");
            }
        }
    }

    fn loop_block(&mut self) {
        if !match_token(self.parser.parse_next(), Token::TkOpenBracket) {
            panic!("Expected '{{' at the beginning of a block");
        }
        let loop_start = self.code.len() + 1;
        self.begin_scope();
        self.emit_loop(0);
        loop {
            if let Some(token) = self.parser.peek() {
                match token {
                    //TODO: make a new instruction for instr idx instead of constant idx
                    Token::TkOpenBracket => self.block(),
                    Token::TkCloseBracket => {
                        self.parser.parse_next();
                        break;
                    }
                    _ => {
                        if self.statement() {
                            continue;
                        }
                        panic!("expected a '}}' to end the loop");
                    }
                }
            }
        }
        self.code[loop_start] = Instruction::from_instruction_idx(self.code.len() + 2);
        self.emit_jump(loop_start);
        self.end_scope();
    }

    pub fn statement(&mut self) -> bool {
        if let Some(token) = self.parser.peek() {
            match token {
                Token::TkNum | Token::TkString | Token::TkTrue | Token::TkFalse => {
                    self.expression_statement(None);
                }
                Token::TkFor => todo!(),
                Token::TkIdentifier => self.variable(),
                Token::TkPrint => self.print_statement(),
                // statements that dont require semicolons at the end
                Token::TkIf => {
                    self.if_statement();
                    return true;
                }
                Token::TkLoop => {
                    self.loop_statement();
                    return true;
                }

                Token::TkBreak => {
                    self.parser.parse_next();
                    self.parser.parse_next();
                    self.emit_operation(Operations::OpBreak);
                    return true;
                }

                Token::TkContinue => {
                    self.parser.parse_next();
                    self.parser.parse_next();
                    self.emit_operation(Operations::OpContinue);
                    return true;
                }
                Token::TkElse => panic!("else statements must be preceded by an 'if' "),
                Token::TkPlus
                | Token::TkMinus
                | Token::TkStar
                | Token::TkSlash
                | Token::TkErr
                | Token::TkSemicolon
                | Token::TkOr
                | Token::TkAnd
                | Token::TkEquals
                | Token::TkLessThan
                | Token::TkGreaterThan
                | Token::TkDoubleEquals
                | Token::TkEof
                | Token::TkOpenBracket
                | Token::TkCloseBracket
                | Token::TkOpenParen
                | Token::TkCloseParen => {
                    panic!("expected a statement or expression");
                }
            }
            if let Some(maybe_semicolon) = self.parser.parse_next() {
                match maybe_semicolon {
                    Token::TkSemicolon => return true,
                    _ => panic!(
                        "expected a semicolon at the end of a statement, found {:?}",
                        maybe_semicolon
                    ),
                }
            } else {
                panic!("expected semicolon, found EOF"); // EOF expected semicolon
            }
        } else {
            return false; // EOF no error
        }
    }

    fn loop_statement(&mut self) {
        self.parser.parse_next(); // consume the 'loop' word
        if match_token(self.parser.peek(), Token::TkFor) {
            self.loop_for_statement();
            return;
        }
        self.loop_block();
    }

    fn loop_for_statement(&mut self) {
        self.parser.parse_next(); // consume the 'for'
    }

    fn if_statement(&mut self) {
        self.parser.parse_next(); // consume the 'if'
        if match_token(self.parser.parse_next(), Token::TkOpenParen) {
            if self.expression(None, Token::TkCloseParen) == ValType::ValBoolType {
                assert!(
                    match_token(self.parser.parse_next(), Token::TkCloseParen),
                    "expected a ')' after the condition"
                );
                assert!(
                    match_token(self.parser.peek(), Token::TkOpenBracket),
                    "expected '{{' after the condition block in if statement"
                );
                self.emit_jif(0);
                let index_of_if = self.code.len() - 1;
                self.block();
                self.code[index_of_if] = Instruction::from_instruction_idx(self.code.len() + 2);
                let index_of_else = self.code.len() - 1;
                self.emit_jump(0); // jump to the end of the else statement
                if match_token(self.parser.peek(), Token::TkElse) {
                    self.parser.parse_next();
                    self.block();
                    self.code[index_of_else + 2] =
                        Instruction::from_instruction_idx(self.code.len());
                }
            } else {
                panic!("the condition inside of the condition block evaluate to a boolean");
            }
        } else {
            panic!("expected a condition wrapped in parentheses | if (...boolean) |");
        }
    }

    fn print_statement(&mut self) {
        self.parser.parse_next(); // consume the print statement
        self.expression(None, Token::TkSemicolon);
        self.emit_operation(Operations::OpPrint);
    }

    fn has_variable(&self, name: &str) -> Option<usize> {
        let mut idx: usize = self.locals.len();
        let mut local_height = self.scope_depth;
        let local_reverse_iter = self.locals.iter().rev();
        for local in local_reverse_iter {
            // if true, this variable is in a scope seperate and not inclusive of the current
            // scope
            if local.depth > local_height {
                break;
            }
            local_height = local.depth;
            if local.name == name {
                return Some(idx - 1);
            }
            idx -= 1;
        }
        None
    }

    fn variable(&mut self) {
        let maybe_operand = self.parser.parse_next(); // parse the identifier. If this turns out to be an expression
                                                      // statement, we have to let it know that we already parsed one of its operands, oops!
        let var_name = self.parser.get_curr_slice();
        if let Some(type_ident) = self.types.get(var_name) {
            return self.variable_declaration(match_val_type(type_ident));
        }

        // if this isnt a var dec, check that we know the variable
        let local_idx = match self.has_variable(var_name) {
            Some(local_idx) => local_idx,
            None => panic!("unknown variable {}", var_name),
        };
        if let Some(maybe_equals) = self.parser.peek() {
            match maybe_equals {
                Token::TkEquals => {
                    self.parser.parse_next(); // consume the equals sign
                    self.assignment(local_idx);
                }
                _ => {
                    self.expression_statement(maybe_operand.as_ref());
                }
            }
        }
    }

    pub fn variable_declaration(&mut self, var_type: ValType) -> () {
        self.parser.parse_next(); // identifier
        let name = self.parser.get_curr_slice().to_owned();
        if let Some(_) = self.has_variable(&name) {
            panic!(
                "A variable with the name {} already exists in this scope",
                name
            );
        }
        if match_token(self.parser.parse_next(), Token::TkEquals) {
            let return_type = self.expression(None, Token::TkSemicolon);
            if var_type != return_type {
                panic!("cannot assign type {:?} to {:?}", var_type, return_type);
            }
            let local = Local::new(String::from(name), self.scope_depth, var_type);
            self.locals.push(local);
        } else {
            panic!("expected an equals sign to declare the variable");
            //TODO: allow uninitialized vars?
        }
    }

    fn assignment(&mut self, local_idx: usize) {
        let return_type: ValType = self.expression(None, Token::TkSemicolon);
        assert_eq!(
            return_type, self.locals[local_idx].val_type,
            "cannot set a variable of type {:?} to {:?}",
            self.locals[local_idx].val_type, return_type
        );
        self.emit_set_local(local_idx);
    }

    pub fn expression_statement(&mut self, maybe_parsed_operand: Option<&Token>) {
        self.expression(maybe_parsed_operand, Token::TkSemicolon);
        self.emit_operation(Operations::OpPop);
    }

    pub fn expression(
        &mut self,
        maybe_parsed_operand: Option<&Token>,
        expected_end_token: Token,
    ) -> ValType {
        let mut operator_stack: Vec<Operations> = Vec::new();
        let mut operand_type_stack: Vec<ValType> = Vec::new();
        let mut operand_phase: bool = true;
        if let Some(parsed_operand) = maybe_parsed_operand {
            self.compile_operand(parsed_operand, &mut operand_type_stack);
            operand_phase = false;
        }
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
                        if self.dump_stack(&mut operator_stack, &mut operand_type_stack) {
                            continue;
                        } else if match_token(self.parser.peek(), expected_end_token) {
                            break;
                        } else {
                            panic!("unmatched closing parentheses");
                        }
                    }
                    // compile constant
                    if operand_phase {
                        self.compile_operand(&token, &mut operand_type_stack);
                        operand_phase = false;
                        if match_token(self.parser.peek(), expected_end_token) {
                            self.dump_stack(&mut operator_stack, &mut operand_type_stack);
                            break;
                        }
                    }
                    // compile operator
                    else {
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
                }
                None => {
                    self.dump_stack(&mut operator_stack, &mut operand_type_stack);
                    break;
                }
            }
        }
        assert_eq!(operand_type_stack.len(), 1);
        return *operand_type_stack.get(0).unwrap();
    }

    fn assert_is_constant(&self, token: &Token) -> () {
        use Token::*;
        assert!(match token {
            TkNum | TkString | TkTrue | TkFalse | TkIdentifier => true,
            _ => false,
        })
    }

    fn compile_operand(&mut self, token: &Token, operand_type_stack: &mut Vec<ValType>) {
        self.assert_is_constant(token);
        // if this is a variable, get local, else emit constant
        if token == &Token::TkIdentifier {
            let ident = self.parser.get_curr_slice();
            let idx = self.has_variable(ident).unwrap();
            self.emit_get_local(idx);
            push_type(self.locals[idx].val_type, operand_type_stack);
        } else {
            self.emit_constant(token);
            let new_val_ref = self.constants.last().unwrap();
            push_type_of_val(new_val_ref, operand_type_stack);
        }
    }

    fn dump_stack(
        &mut self,
        stack: &mut Vec<Operations>,
        operand_type_stack: &mut Vec<ValType>,
    ) -> bool {
        // dump until we hit a grouing operation, then pop that and exit
        while stack.len() > 0 {
            if let Some(operation) = stack.pop() {
                match operation {
                    Operations::OpGrouping => {
                        return true;
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
                            Operations::OpLessThan
                            | Operations::OpGreaterThan
                            | Operations::OpEquals => {
                                if can_compare(operand_1, operand_2, operand_type_stack) {
                                    self.emit_operation(operation);
                                } else {
                                    panic!("Type mismatch: can only use comparison operators with compatible types, found {:?}, and {:?}", operand_1, operand_2);
                                }
                            }
                            Operations::OpConstant
                            | Operations::OpPrint
                            | Operations::OpSetLocal
                            | Operations::OpGetLocal
                            | Operations::NoOp
                            | Operations::OpGrouping
                            | Operations::OpPop
                            | Operations::OpJump
                            | Operations::OpLoop
                            | Operations::OpBreak
                            | Operations::OpContinue
                            | Operations::OpJumpIfFalse => (),
                        }
                    }
                }
            }
        }
        return false;
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
        compiler.expression(None, Token::TkSemicolon);

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
        compiler.expression(None, Token::TkEof);

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
        compiler.expression(None, Token::TkEof);
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
        compiler.expression(None, Token::TkEof);
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
        compiler.expression(None, Token::TkEof);
    }

    #[test]
    #[should_panic]
    fn panic_multiplying_num_and_bool() {
        let code: &String = &String::from("1 * false + 2");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression(None, Token::TkEof);
    }

    #[test]
    fn compile_a_string_literal() {
        let mut my_string = String::from('"');
        my_string.push_str("hello");
        my_string.write_char('"').unwrap();
        let mut compiler: Compiler = Compiler::new(&my_string);
        compiler.expression(None, Token::TkEof);

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
        compiler.expression(None, Token::TkEof);

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

    #[test]
    fn compile_set_local() {
        let code = String::from("int myNumber = 10; myNumber =  20;");
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
                Instruction::from_operation(OpSetLocal),
                Instruction::from_constant_idx(0),
            ]
        )
    }

    #[test]
    fn compile_print_statement() {
        let code = String::from("String myString = \"hello!\"; print myString; ");
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
                Instruction::from_operation(OpPrint),
            ]
        )
    }

    #[test]
    fn compile_local_strings() {
        let code = String::from("String benji = \"benji\"; String camille = \" camille\"; String both = benji + camille; print both; ");

        let mut compiler = Compiler::new(&code);

        compiler.statement();
        compiler.statement();
        compiler.statement();
        compiler.statement();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpConcat),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpPrint)
            ]
        );
    }

    #[test]
    #[should_panic]
    fn wrong_type_dec() {
        let code = String::from("String myString = 12;");
        let mut compiler = Compiler::new(&code);
        compiler.statement();
    }

    #[test]
    #[should_panic]
    fn wrong_type_assignment() {
        let code = String::from("String myString = \"hello\"; myString = 12");
        let mut compiler = Compiler::new(&code);
        compiler.statement();
        compiler.statement();
    }

    #[test]
    #[should_panic]
    fn wrong_type_assignment_2() {
        let code =
            String::from("String myString = \"hello\"; int myNumber = 12; myString = myNumber");
        let mut compiler = Compiler::new(&code);
        compiler.statement();
        compiler.statement();
        compiler.statement();
    }

    #[test]
    fn compile_a_block() {
        let code = String::from("int one = 1; {String hello = \"hello\";} print one;");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpPop), // pop off the hello
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpPrint),
            ]
        );

        assert_eq!(
            &compiler.constants,
            &vec![Value::from_num(1), Value::from_string("hello")]
        );
    }

    #[test]
    fn compare_numbers() {
        let code = String::from("int one = 1; int two = 2; one == two; one < two;");
        let mut compiler = Compiler::new(&code);

        compiler.compile();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpPop),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpLessThan),
                Instruction::from_operation(OpPop),
            ]
        );
    }

    #[test]
    fn compare_strings() {
        let code = String::from("String one = \"hello\"; String two = \"world\"; one == two;");
        let mut compiler = Compiler::new(&code);

        compiler.compile();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpPop),
            ]
        );
    }
    #[test]
    fn compare_booleans() {
        let code = String::from("bool one = true; bool two = false; one == two;");
        let mut compiler = Compiler::new(&code);

        compiler.compile();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpGetLocal),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpPop),
            ]
        );
    }

    #[test]
    #[should_panic]
    fn compare_different_types() {
        let code = String::from("bool t = true; int one = 1; t == one;");
        let mut compiler = Compiler::new(&code);

        compiler.compile();
    }

    #[test]
    fn compile_if_statement() {
        let code = String::from("if (1 == 2){ print \"hello\"; }");
        let mut compiler = Compiler::new(&code);

        compiler.compile();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpJumpIfFalse),
                Instruction::from_constant_idx(10),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpPop),
            ]
        )
    }

    #[test]
    fn compile_if_else_statement() {
        let code = String::from("if (1 == 2){ print \"hello\"; } else {print 1;}");
        let mut compiler = Compiler::new(&code);

        compiler.compile();

        assert_eq!(
            &compiler.code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpJumpIfFalse),
                Instruction::from_constant_idx(10),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(3),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpPop),
            ]
        )
    }
}
