use core::fmt;
use core::panic;
use std::collections::HashSet;
use std::rc::Rc;

use crate::object::ObjFunction;
use crate::object::ObjString;
use crate::value;
use crate::value::ValType;
use crate::{
    parser::{Parser, Token},
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
    OpGreaterEquals,
    OpLessEquals,
    OpNot,
    OpNotEquals,
    OpJump,
    OpEquals,
    OpPop,
    OpLoop,
    OpBreak,
    OpLoopFor,
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

pub enum ExprResult {
    ParsedOperator(Operations),
    ParsedOperand,
    Assignment(ValType),
    Done(ValType),
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
        OpLoopFor => PrecNone,
        OpNot => PrecUnary,
        OpNotEquals => PrecEquality,
        OpLessEquals => PrecComparison,
        OpGreaterEquals => PrecComparison,
    }
}

fn top_of<T>(stack: &[T]) -> Option<&T> {
    let length = stack.len();
    if length == 0 {
        None
    } else {
        Some(&stack[length - 1])
    }
}

fn is_group_start(token: &Token) -> bool {
    matches!(token, Token::TkOpenParen)
}

fn is_group_end(token: &Token) -> bool {
    matches!(token, Token::TkCloseParen)
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
    false
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
    false
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
    false
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
    false
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
    false
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

pub struct Chunk {
    pub code: Vec<Instruction>,
    pub constants: Vec<Value>,
    locals: Vec<Local>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::<Instruction>::new(),
            constants: Vec::<Value>::new(),
            locals: Vec::<Local>::new(),
        }
    }
}

pub struct Compiler<'a> {
    parser: Parser<'a>,
    pub function_stack: Vec<ObjFunction>,
    types: HashSet<String>,
    scope_depth: u8,
}

impl<'a> Compiler<'a> {
    pub fn new(code: &'a str) -> Self {
        let mut types = HashSet::new();
        types.insert(String::from("int"));
        types.insert(String::from("bool"));
        types.insert(String::from("String"));
        Compiler {
            parser: Parser::new(code),
            function_stack: vec![ObjFunction::new(crate::object::FunctionType::Script)],
            types,
            scope_depth: 0,
        }
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.function_stack.last_mut().unwrap().chunk
    }
    fn current_chunk_ref(&self) -> &Chunk {
        &self.function_stack.last().unwrap().chunk
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
        match token {
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
            TkGreaterThan => OpGreaterThan,
            TkDoubleEquals => OpEquals,
            TkLessEquals => OpLessEquals,
            TkGreaterEquals => OpGreaterEquals,
            TkBang => OpNot,
            TkNotEquals => OpNotEquals,
            _ => {
                panic!("Expected an operator token, got {:?}", token);
            }
        }
    }

    fn make_constant(&mut self, val: Value) -> Result<u8, &'static str> {
        let idx = self.current_chunk_ref().constants.len();
        self.current_chunk_mut().constants.push(val);
        Ok(idx.try_into().unwrap())
    }

    fn emit_jif(&mut self, idx: usize) {
        self.current_chunk_mut()
            .code
            .push(Instruction::from_operation(Operations::OpJumpIfFalse));
        self.current_chunk_mut()
            .code
            .push(Instruction::from_instruction_idx(idx));
    }

    fn emit_jump(&mut self, idx: usize) {
        self.current_chunk_mut()
            .code
            .push(Instruction::from_operation(Operations::OpJump));
        self.current_chunk_mut()
            .code
            .push(Instruction::from_instruction_idx(idx));
    }

    fn emit_loop(&mut self, idx: usize) {
        self.current_chunk_mut()
            .code
            .push(Instruction::from_operation(Operations::OpLoop));
        self.current_chunk_mut()
            .code
            .push(Instruction::from_instruction_idx(idx));
    }

    fn emit_constant(&mut self, token: &Token) {
        let val: Value = self.token_to_val(token);

        if let Ok(idx) = self.make_constant(val) {
            self.current_chunk_mut()
                .code
                .push(Instruction::Operation(Operations::OpConstant));
            self.current_chunk_mut()
                .code
                .push(Instruction::ConstantIdx(idx));
        } else {
            panic!("error in alocating constant");
        }
    }

    fn emit_get_local(&mut self, idx: usize) {
        self.current_chunk_mut()
            .code
            .push(Instruction::from_operation(Operations::OpGetLocal));
        self.current_chunk_mut()
            .code
            .push(Instruction::from_constant_idx(idx.try_into().unwrap()));
    }

    fn emit_set_local(&mut self, idx: usize) {
        self.current_chunk_mut()
            .code
            .push(Instruction::from_operation(Operations::OpSetLocal));
        self.current_chunk_mut()
            .code
            .push(Instruction::from_constant_idx(idx.try_into().unwrap()));
    }

    fn emit_operation(&mut self, operation: Operations) {
        self.current_chunk_mut()
            .code
            .push(Instruction::Operation(operation));
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
                            return;
                        }
                    }
                };
            } else {
                return;
            }
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let scope_depth = self.scope_depth;
        let mut count = self.current_chunk_ref().locals.len();
        let local_rev_iter = self.current_chunk_ref().locals.iter().rev();
        for local in local_rev_iter {
            if local.depth < scope_depth {
                break;
            }
            count -= 1;
        }
        let local_len = self.current_chunk_ref().locals.len();
        for _ in 0..(local_len - count) {
            // pop all the locals off the stack that no longer exist
            self.emit_operation(Operations::OpPop);
        }
        self.current_chunk_mut().locals.drain(count..); // remove the locals from the locals array
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

    pub fn statement(&mut self) -> bool {
        if let Some(token) = self.parser.peek() {
            match token {
                Token::TkNum
                | Token::TkString
                | Token::TkTrue
                | Token::TkFalse
                | Token::TkOpenParen
                | Token::TkBang => {
                    self.expression_statement();
                }
                Token::TkFor => todo!(),
                Token::TkTypeIdent => self.variable_declaration(),
                Token::TkIdentifier => self.expression_statement(),
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
                | Token::TkNotEquals
                | Token::TkLessEquals
                | Token::TkGreaterEquals
                | Token::TkCloseParen => {
                    panic!("expected a statement or expression");
                }
            }

            //TODO: a more elegant way of searching for a function dec, which doesnt require a
            //semicolon?
            if self.current_chunk_ref().constants.last().unwrap().val_type == ValType::ValFunctionType {
                return true;
            }
            if let Some(maybe_semicolon) = self.parser.parse_next() {
                match maybe_semicolon {
                    Token::TkSemicolon => true,
                    _ => panic!(
                        "expected a semicolon at the end of a statement, found {:?}",
                        maybe_semicolon
                    ),
                }
            } else {
                panic!("expected semicolon, found EOF"); // EOF expected semicolon
            }
        } else {
            false // EOF no error
        }
    }

    fn b_loop(&mut self) -> usize {
        // parse open bracket
        if !match_token(self.parser.parse_next(), Token::TkOpenBracket) {
            panic!("Expected '{{' at the beginning of a block");
        }
        // store the beginning index of the loop
        let loop_start = self.current_chunk_ref().code.len() + 1;
        self.begin_scope();
        self.emit_loop(0);
        loop {
            if let Some(token) = self.parser.peek() {
                match token {
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
        self.end_scope();

        loop_start
    }

    fn loop_block(&mut self) -> usize {
        // for regular loops, emit instructions for loop, followed by jump to start
        let loop_start = self.b_loop();
        self.emit_jump(loop_start);

        loop_start
    }

    fn loop_statement(&mut self) {
        self.parser.parse_next(); // consume the 'loop' word
        let loop_start;
        if match_token(self.parser.peek(), Token::TkFor) {
            loop_start = self.loop_for_statement();
        } else {
            loop_start = self.loop_block();
        }

        // update the loop instruction to contain the end of the loop
        self.current_chunk_mut().code[loop_start] =
            Instruction::from_instruction_idx(self.current_chunk_ref().code.len());
    }

    fn loop_for_statement(&mut self) -> usize {
        self.parser.parse_next(); // consume the 'for'
        let loop_count = self.expression(Token::TkOpenBracket); // int count left on stack
        if loop_count != ValType::ValNumType {
            panic!("loop for count must be an integer");
        }
        self.emit_operation(Operations::OpLoopFor);
        self.b_loop()
    }

    fn if_statement(&mut self) {
        self.parser.parse_next(); // consume the 'if'
        // parse the condition
        if match_token(self.parser.parse_next(), Token::TkOpenParen) {
            if self.expression(Token::TkCloseParen) == ValType::ValBoolType {
                assert!(
                    match_token(self.parser.parse_next(), Token::TkCloseParen),
                    "expected a ')' after the condition"
                );
                assert!(
                    match_token(self.parser.peek(), Token::TkOpenBracket),
                    "expected '{{' after the condition block in if statement"
                );
                // jump instruction for the end of the if statement
                // then store this index in order to update the instruction once we know the ending
                // index 
                self.emit_jif(0);
                let index_of_if = self.current_chunk_ref().code.len() - 1;

                // compile if block
                self.block();
                

                self.current_chunk_mut().code[index_of_if] =
                    Instruction::from_instruction_idx(self.current_chunk_ref().code.len() + 2); // jump
                                                                                                // to the end of the if block, including the jump instruction
                let index_of_else = self.current_chunk_ref().code.len() - 1;
                self.emit_jump(0); // jump to the end of the else statement
                if match_token(self.parser.peek(), Token::TkElse) {
                    self.parser.parse_next();
                    self.block();
                }
                // jump to the end of the condition, including the else, if applicable
                self.current_chunk_mut().code[index_of_else + 2] =
                    Instruction::from_instruction_idx(self.current_chunk_ref().code.len());
            } else {
                panic!("the condition inside of the condition block must evaluate to a boolean");
            }
        } else {
            panic!("expected a condition wrapped in parentheses | if (...boolean) |");
        }
    }

    fn print_statement(&mut self) {
        self.parser.parse_next(); // consume the print statement
        self.expression(Token::TkSemicolon);
        self.emit_operation(Operations::OpPrint);
    }

    fn has_variable(&self, name: &str) -> Option<usize> {
        let mut idx: usize = self.current_chunk_ref().locals.len();
        let mut local_height = self.scope_depth;
        let local_reverse_iter = self.current_chunk_ref().locals.iter().rev();
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

    pub fn variable_declaration(&mut self) {
        self.parser.parse_next(); // type ident
        let var_type = match_val_type(self.parser.get_curr_slice()); // grab type name
        self.parser.parse_next(); // identifier
        let name = self.parser.get_curr_slice().to_owned(); // grab indent name
        if self.has_variable(&name).is_some() {
            panic!(
                "A variable with the name {} already exists in this scope",
                name
            );
        }
        // if its an equals sign, this is a new variable
        // if its a parentheses, this is a function declaration
        match self.parser.parse_next().unwrap() { 
            Token::TkEquals => {
                let return_type = self.expression(Token::TkSemicolon);
                if var_type != return_type {
                    panic!("cannot assign type {:?} to {:?}", var_type, return_type);
                }
                let local = Local::new(name, self.scope_depth, var_type);
                self.current_chunk_mut().locals.push(local);
            }
            Token::TkOpenParen => {
                let function_local = Local::new(name, self.scope_depth, ValType::ValFunctionType);
                self.current_chunk_mut().locals.push(function_local);
                //TODO:: function needs to know its own name?
                let mut function = ObjFunction::new(crate::object::FunctionType::Function);
                let parameters: Vec<ValType> = self.parse_function_params(&mut function);
                function.set_params(parameters);

                // compile the function, and once done, put it in this the enclosing function's
                // constants table
                function = self.compile_function(function);
                let _ = self.make_constant(Value { val_type: ValType::ValFunctionType, data: ValData::ValObj(Rc::new(function))});
            }
            _ => panic!("Expected a '=' or an open parentheses"),
        }
    }
    
    fn compile_function(&mut self, new_function: ObjFunction) -> ObjFunction{
        // takes a function and gives it back 
        self.function_stack.push(new_function);
        self.block();
        self.function_stack.pop().unwrap()
    }


    fn parse_function_params(&mut self, function: &mut ObjFunction) -> Vec<ValType> {
        let mut parameters = Vec::new();
        loop {
            match self.parser.parse_next() {
                Some(token) => match token {
                    Token::TkCloseParen => break,
                    Token::TkTypeIdent => {
                        parameters.push(ValType::from_str(self.parser.get_curr_slice()));
                        if !match_token(self.parser.parse_next(), Token::TkIdentifier) {
                            panic!("expected the name of an identifier");
                        }
                        let local = Local::new(
                            self.parser.get_curr_slice().to_string(),
                            0,
                            *parameters.last().unwrap(),
                        );
                        self.current_chunk_mut().locals.push(local);
                    }
                    _ => (),
                },
                None => panic!("expected a function argument or a close parens ')'"),
            }
        }
        parameters
    }

    pub fn expression_statement(&mut self) {
        self.expression(Token::TkSemicolon);

        // for set expression, we already the stack to move the value of the expression into the
        // local slot
        let idx = self.current_chunk_ref().code.len() - 2;
        if self.current_chunk_mut().code[idx]
            == Instruction::from_operation(Operations::OpSetLocal)
        {
            return;
        }

        //otherwise, pop the result manually
        self.emit_operation(Operations::OpPop);
    }

    fn assignment(&mut self) -> ValType {
        let var_name = self.parser.get_curr_slice();
        let local_idx = self.has_variable(var_name).unwrap();
        self.parser.parse_next(); // consume the equals sign
        let return_type: ValType = self.expression(Token::TkSemicolon);
        println!("returned from expression with {:?}", return_type);
        assert_eq!(
            return_type,
            self.current_chunk_ref().locals[local_idx].val_type,
            "cannot set a variable of type {:?} to {:?}",
            self.current_chunk_ref().locals[local_idx].val_type,
            return_type
        );
        self.emit_set_local(local_idx);
        return_type
    }

    fn expr_first_token(
        &mut self,
        operand_type_stack: &mut Vec<ValType>,
        expected_end_token: Token,
    ) -> ExprResult {
        // the result of parsing the first token can be three distinct things.
        //
        // 1. a unary prefix operator -> just add the op to the stack
        //    [ExprResult::ParsedOperator(unary)].
        //
        // 2. constant followed by an equals sign -> reroute to assignment [ExprResult::assignment]
        //
        // 3. constant followed by end token -> compile constant and then exit [ExprResult::Done]
        //
        //4. beginning of a grouping -> push OpGrouping [ExprResult::ParsedOperator(grouping)]
        //
        //5. operand followed by an operator [ExprResult::ParsedOperator]
        if let Some(first_token) = self.parser.parse_next() {
            match first_token {
                Token::TkBang => ExprResult::ParsedOperator(Operations::OpNot),
                Token::TkIdentifier
                | Token::TkNum
                | Token::TkString
                | Token::TkTrue
                | Token::TkFalse => {
                    if match_token(self.parser.peek(), Token::TkEquals) {
                        ExprResult::Assignment(self.assignment())
                    } else {
                        self.compile_operand(&first_token, operand_type_stack);
                        // check for the end of the expression.
                        if match_token(self.parser.peek(), expected_end_token) {
                            ExprResult::Done(*operand_type_stack.first().unwrap())
                        } else {
                            ExprResult::ParsedOperand
                        }
                    }
                }
                Token::TkOpenParen => {
                    println!("first token was an open paren");
                    ExprResult::ParsedOperator(Operations::OpGrouping)
                }
                _ => panic!("unexpected beginnging to expression: {:?}", first_token),
            }
        } else {
            panic!("expected an expression");
        }
    }

    fn expr_check_group_end(
        &mut self,
        operator_stack: &mut Vec<Operations>,
        operand_type_stack: &mut Vec<ValType>,
        expected_end_token: Token,
    ) -> Option<ExprResult> {
        // after each operand phase, check for the end of the statement
        // could be a semicolon or a close paren.
        //
        // case 1: not a close paren, break and do nothing
        //
        // case 2: it is a close parens, but close paren is not the expected
        // end -> dump stack, consume the close paren, panic if unmatched grouping, then loop to close
        // all close parens that follow
        //
        // case 3: it is a close parens, and we were expecting a close parens
        // end token, but this one closes properly -> dump the stack, consume
        // the close paren, then loop
        //
        // case 4: it is a close parens, we were expecting a close parens end
        // token, and the close parens is unmatched -> dump all, return from
        // expression
        loop {
            if match_token(self.parser.peek(), Token::TkCloseParen) {
                let did_close = self.dump_stack(operator_stack, operand_type_stack, true);
                if expected_end_token == Token::TkCloseParen {
                    if !did_close {
                        self.dump_all(operator_stack, operand_type_stack);
                        assert_eq!(operand_type_stack.len(), 1);
                        assert_eq!(operator_stack.len(), 0);
                        return Some(ExprResult::Done(*operand_type_stack.first().unwrap()));
                    }
                } else {
                    if !did_close {
                        panic!("unmatched close parentheses");
                    }
                }
                self.parser.parse_next();
            } else {
                return None;
            }
        }
    }

    pub fn expression(&mut self, expected_end_token: Token) -> ValType {
        let mut operator_stack: Vec<Operations> = Vec::new();
        let mut operand_type_stack: Vec<ValType> = Vec::new();
        let mut operand_phase: bool = true;

        // consume the first token of the expression.
        // it could be either an operand, an open paren, or an operator (unary)
        // if its an operand, check to seee if this is an assignment by peeking
        // to see if the next token is an equals sign
        // otherwise, continue the expression as normal

        match self.expr_first_token(&mut operand_type_stack, expected_end_token) {
            ExprResult::ParsedOperand => operand_phase = false,
            ExprResult::ParsedOperator(op) => operator_stack.push(op),
            ExprResult::Done(val_type) => return val_type,
            ExprResult::Assignment(val_type) => return val_type,
        }
        loop {
            let maybe_token = self.parser.parse_next();
            match maybe_token {
                Some(token) => {
                    // check if the token is a grouping
                    if is_group_start(&token) {
                        operator_stack.push(Operations::OpGrouping);
                        continue;
                    }
                    // compile constant
                    if operand_phase {
                        // if this is a !, compile the OpNot and move next
                        if token == Token::TkBang {
                            operator_stack.push(Operations::OpNot);
                            continue;
                        }
                        self.compile_operand(&token, &mut operand_type_stack);
                        operand_phase = false;

                        match self.expr_check_group_end(
                            &mut operator_stack,
                            &mut operand_type_stack,
                            expected_end_token,
                        ) {
                            Some(result) => match result {
                                ExprResult::Done(val_type) => return val_type,
                                _ => panic!("unexpected result"),
                            },
                            None => (),
                        }

                        // if it is not a close parens but it is an expected end token, simply
                        // return from the expression
                        if match_token(self.parser.peek(), expected_end_token) {
                            self.dump_all(&mut operator_stack, &mut operand_type_stack);
                            break;
                        }
                    } else {
                        let operator =
                            self.token_to_operator(&token, top_of(&operand_type_stack).unwrap());
                        let top_of_operator_stack: &Operations =
                            top_of(&operator_stack).unwrap_or(&Operations::NoOp);

                        // if the new operator is of a higher precedence than the last, push it onto the
                        // stack
                        if prec_of(&operator) > prec_of(top_of_operator_stack) {
                            operator_stack.push(operator);
                        } else {
                            self.dump_stack(&mut operator_stack, &mut operand_type_stack, false);
                            operator_stack.push(operator);
                        }
                        operand_phase = true;
                    }
                }
                None => {
                    self.dump_all(&mut operator_stack, &mut operand_type_stack);
                    break;
                }
            }
        }
        assert_eq!(operand_type_stack.len(), 1);
        assert_eq!(operator_stack.len(), 0);
        return *operand_type_stack.first().unwrap();
    }

    fn assert_is_constant(&self, token: &Token) {
        use Token::*;
        assert!(matches!(
            token,
            TkNum | TkString | TkTrue | TkFalse | TkIdentifier
        ));
    }

    fn compile_operand(&mut self, token: &Token, operand_type_stack: &mut Vec<ValType>) {
        self.assert_is_constant(token);
        // if this is a variable, get local, else emit constant
        if token == &Token::TkIdentifier {
            let ident = self.parser.get_curr_slice();
            let idx = self.has_variable(ident).unwrap();
            self.emit_get_local(idx);
            push_type(
                self.current_chunk_ref().locals[idx].val_type,
                operand_type_stack,
            );
        } else {
            self.emit_constant(token);
            let new_val_ref = self.current_chunk_ref().constants.last().unwrap();
            push_type_of_val(new_val_ref, operand_type_stack);
        }
    }

    fn dump_all(&mut self, stack: &mut Vec<Operations>, operand_type_stack: &mut Vec<ValType>) {
        loop {
            if !self.dump_stack(stack, operand_type_stack, false) || stack.is_empty() {
                break;
            }
        }
    }

    fn dump_stack(
        &mut self,
        stack: &mut Vec<Operations>,
        operand_type_stack: &mut Vec<ValType>,
        dump_group: bool,
    ) -> bool {
        if stack.is_empty() {
            return false;
        }
        // dump until we hit a grouing operation, then pop that and exit
        while !stack.is_empty() {
            if let Some(operation) = stack.pop() {
                match operation {
                    Operations::OpGrouping => {
                        if dump_group {
                            return true;
                        } else {
                            stack.push(Operations::OpGrouping);
                            return false;
                        }
                    }
                    Operations::OpNot => {
                        if let Some(operand) = operand_type_stack.pop() {
                            match operand {
                                ValType::ValBoolType => {
                                    operand_type_stack.push(ValType::ValBoolType);
                                    self.emit_operation(Operations::OpNot);
                                }
                                _ => panic!(
                                    "Can only apply the '!' operator to a boolean, receieved {:?}",
                                    operand
                                ),
                            }
                        }
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
                            Operations::OpAnd | Operations::OpOr | Operations::OpNot => {
                                if can_and_or_or(operand_1, operand_2, operand_type_stack) {
                                    self.emit_operation(operation);
                                } else {
                                    panic!("Type mismatch: can only use 'and' and 'or' operators with booleans, found {:?}, and {:?}", operand_1, operand_2);
                                }
                            }
                            Operations::OpLessThan
                            | Operations::OpGreaterThan
                            | Operations::OpLessEquals
                            | Operations::OpGreaterEquals
                            | Operations::OpNotEquals
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
                            | Operations::OpLoopFor
                            | Operations::OpBreak
                            | Operations::OpContinue
                            | Operations::OpJumpIfFalse => (),
                        }
                    }
                }
            }
        }
        false
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
        compiler.expression(Token::TkSemicolon);

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ]
        );

        assert_eq!(
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_num(123)]
        );
    }

    #[test]
    fn compile_an_arithmatic_expression() {
        let code: &String = &String::from("1 + (2 * 3)");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression(Token::TkEof);

        assert_eq!(
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_num(1), Value::from_num(2), Value::from_num(3)]
        );
    }
    #[test]
    fn compile_a_boolean() {
        let code: &String = &String::from("true");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression(Token::TkEof);
        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ],
        );

        assert_eq!(
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_bool(true)]
        )
    }

    #[test]
    fn compile_a_boolean_operation() {
        let code: &String = &String::from("true and (false or false) and true");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression(Token::TkEof);
        assert_eq!(
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().constants,
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
        compiler.expression(Token::TkEof);
    }

    #[test]
    #[should_panic]
    fn panic_multiplying_num_and_bool() {
        let code: &String = &String::from("1 * false + 2");
        let mut compiler: Compiler = Compiler::new(code);
        compiler.expression(Token::TkEof);
    }

    #[test]
    fn compile_a_string_literal() {
        let mut my_string = String::from('"');
        my_string.push_str("hello");
        my_string.write_char('"').unwrap();
        let mut compiler: Compiler = Compiler::new(&my_string);
        compiler.expression(Token::TkEof);

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ],
        );

        assert_eq!(
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_string("hello"),]
        );
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
        compiler.expression(Token::TkEof);

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpConcat),
            ],
        );

        assert_eq!(
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_string("hello"), Value::from_string(" world")]
        );
    }

    #[test]
    fn compile_a_local_var_dec() {
        let code = String::from("int myNumber = 1;");
        let mut compiler = Compiler::new(&code);
        compiler.statement();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0)
            ]
        );

        assert_eq!(
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_num(1)]
        );
    }

    #[test]
    fn compile_multiple_statements() {
        let code = String::from("int myNumber = 1; String myString = \"hello\"; ");
        let mut compiler = Compiler::new(&code);
        compiler.statement();
        compiler.statement();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
            ]
        );

        assert_eq!(
            &compiler.current_chunk_ref().constants,
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
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().constants,
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
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_num(1), Value::from_string("hello")]
        );
    }

    #[test]
    fn compare_numbers() {
        let code = String::from("int one = 1; int two = 2; one == two; one < two;");
        let mut compiler = Compiler::new(&code);

        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().code,
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
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpJumpIfFalse),
                Instruction::from_instruction_idx(12),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpJump),
                Instruction::from_instruction_idx(12),
            ]
        )
    }

    #[test]
    fn compile_if_else_statement() {
        let code = String::from("if (1 == 2){ print \"hello\"; } else {print 1;}");
        let mut compiler = Compiler::new(&code);

        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpJumpIfFalse),
                Instruction::from_instruction_idx(12),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpJump),
                Instruction::from_instruction_idx(15),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(3),
                Instruction::from_operation(OpPrint),
            ]
        )
    }

    #[test]
    fn compile_a_loop() {
        let code = String::from("loop { print \"hello\"; }");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpLoop),
                Instruction::from_instruction_idx(7),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpJump),
                Instruction::from_instruction_idx(1),
            ]
        );
    }

    #[test]
    fn compile_a_break() {
        let code = String::from("loop { print \"hello\"; break; }");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpLoop),
                Instruction::from_instruction_idx(8),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpBreak),
                Instruction::from_operation(OpJump),
                Instruction::from_instruction_idx(1),
            ]
        )
    }

    #[test]
    fn compile_nested_break() {
        let code = String::from("loop { if (true) {break;}}");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpLoop),
                Instruction::from_instruction_idx(11),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpJumpIfFalse),
                Instruction::from_instruction_idx(9),
                Instruction::from_operation(OpBreak),
                Instruction::from_operation(OpJump),
                Instruction::from_instruction_idx(9),
                Instruction::from_operation(OpJump),
                Instruction::from_instruction_idx(1),
            ]
        );
    }

    #[test]
    fn compile_loop_for() {
        let code = String::from("loop for (1 + (2 + 4 / 2)) {print \"hello\";}");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(3),
                Instruction::from_operation(OpDivide),
                Instruction::from_operation(OpAdd),
                Instruction::from_operation(OpAdd),
                Instruction::from_operation(OpLoopFor),
                Instruction::from_operation(OpLoop),
                Instruction::from_instruction_idx(17),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(4),
                Instruction::from_operation(OpPrint),
            ]
        )
    }

    #[test]
    fn compile_a_statement_with_group() {
        let code = String::from("print (1);");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpPrint)
            ]
        );
    }
    #[test]
    fn compile_nested_groups() {
        let code = String::from("print (1 + (1 + 1 * 2) * ((1)));");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(3),
                Instruction::from_operation(OpMultiply),
                Instruction::from_operation(OpAdd),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(4),
                Instruction::from_operation(OpMultiply),
                Instruction::from_operation(OpAdd),
                Instruction::from_operation(OpPrint),
            ]
        );
    }

    #[test]
    #[should_panic]
    fn panic_on_unmatched_group_end() {
        let code = String::from("print (1 + (1 + 2)))");
        let mut compiler = Compiler::new(&code);
        compiler.compile();
    }

    #[test]
    #[should_panic]
    fn panic_on_unmatched_group_start() {
        let code = String::from("(1 + ((2 + 1))");
        let mut compiler = Compiler::new(&code);
        compiler.compile();
    }

    #[test]
    fn compile_if_statement_with_groups() {
        let code = String::from("if((2 + 2) == (4 * 1)){print \"success\";}");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpAdd),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(3),
                Instruction::from_operation(OpMultiply),
                Instruction::from_operation(OpEquals),
                Instruction::from_operation(OpJumpIfFalse),
                Instruction::from_instruction_idx(18),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(4),
                Instruction::from_operation(OpPrint),
                Instruction::from_operation(OpJump),
                Instruction::from_instruction_idx(18)
            ]
        );
    }

    #[test]
    fn compile_unary_negation() {
        let code = String::from("!true;");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpNot),
                Instruction::from_operation(OpPop),
            ]
        );
    }

    #[test]
    fn compile_expr_with_unaries() {
        let code = String::from("!(!true or !(false and true));");
        let mut compiler = Compiler::new(&code);
        compiler.compile();

        assert_eq!(
            &compiler.current_chunk_ref().code,
            &vec![
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(0),
                Instruction::from_operation(OpNot),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(1),
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(2),
                Instruction::from_operation(OpAnd),
                Instruction::from_operation(OpNot),
                Instruction::from_operation(OpOr),
                Instruction::from_operation(OpNot),
                Instruction::from_operation(OpPop),
            ]
        );
    }
}
