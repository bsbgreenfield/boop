use crate::compiler::*;
use crate::value;
use core::panic;
use std::rc::Rc;
impl<'a> Compiler<'a> {
    pub fn new(code: &'a str) -> Self {
        let mut types = HashSet::new();
        types.insert(String::from("int"));
        types.insert(String::from("bool"));
        types.insert(String::from("String"));
        Compiler {
            parser: Parser::new(code),
            function_stack: vec![ObjFunction::new(
                crate::object::FunctionType::Script,
                ValType::ValVoidType,
            )],
            types,
            scope_depth: 0,
        }
    }

    pub fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.function_stack.last_mut().unwrap().chunk
    }
    pub fn current_chunk_ref(&self) -> &Chunk {
        &self.function_stack.last().unwrap().chunk
    }

    pub fn token_to_val(&mut self, token: &Token) -> Value {
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

    pub fn token_to_operator(&self, token: &Token, operand_type: &ValType) -> Operations {
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

    pub fn emit_constant_from_token(&mut self, token: &Token) {
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

    fn emit_constant_from_val(&mut self, val: Value) {
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

    pub fn emit_get_local(&mut self, idx: usize) {
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

    fn emit_function_constant(&mut self, function: ObjFunction) {
        if let Ok(idx) = self.make_constant(Value {
            val_type: ValType::ValFunctionType,
            data: ValData::ValObj(Rc::new(function)),
        }) {
            self.current_chunk_mut()
                .code
                .push(Instruction::Operation(Operations::OpConstant));
            self.current_chunk_mut()
                .code
                .push(Instruction::ConstantIdx(idx));
        } else {
            panic!("failed to add function to the constants table");
        }
    }
    
    fn emit_call(&mut self, offset: u8){
       self.current_chunk_mut().code.push(Instruction::from_operation(Operations::OpCall)); 
        self.current_chunk_mut().code.push(Instruction::from_constant_idx(offset));
    }
    pub fn emit_operation(&mut self, operation: Operations) {
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
        if !compiler_functions::match_token(self.parser.parse_next(), Token::TkOpenBracket) {
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
                Token::TkTypeIdent => {
                   if self.variable_declaration() == VarDecResult::FunctionDec {
                        return true;
                    }
                },
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
                Token::TkReturn => {
                    self.return_statement();
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
                    panic!("expected a statement or expression, got {:?}", token );
                }
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

    pub fn has_variable(&self, name: &str) -> Option<usize> {
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

    pub fn variable_declaration(&mut self) -> VarDecResult {
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
                VarDecResult::VariableDec
            }
            Token::TkOpenParen => {
                let function_local = Local::new(name, self.scope_depth, ValType::ValFunctionType);
                println!("PUSHING {:?} to locals", function_local);
                self.current_chunk_mut().locals.push(function_local);
                let mut function =
                    ObjFunction::new(crate::object::FunctionType::Function, var_type);

                // compile the function, and once done, put it in this the enclosing function's
                // constants table
                function = self.compile_function(function);
                self.emit_function_constant(function);
                VarDecResult::FunctionDec
            }
            _ => panic!("Expected a '=' or an open parentheses"),
        }
    }

    fn return_statement(&mut self) {
        // parse return expression and put its value at the top of the stack
        // panic if the value resulting from the expression doesnt match the
        // stated return type of the function

        // consume the return
        self.parser.parse_next();
        let return_type = self.expression(Token::TkSemicolon);
        if return_type != self.function_stack.last().unwrap().return_type {
            panic!(
                "expected a return type of {:?}, got {:?}",
                self.function_stack.last().unwrap().return_type,
                return_type
            );
        }
        self.emit_operation(Operations::OpReturn);
    }

    fn compile_function(&mut self, new_function: ObjFunction) -> ObjFunction {
        // add new function to the top of the function stack
        self.function_stack.push(new_function);
        // parse the function parameters
        let params = self.parse_function_params();
        println!("got {:?} params from function!", params);
        // parse the function body
        self.block();

        // implicit void return
        let function_instructions_rev_iter = self.function_stack.last().unwrap().chunk.code.iter().rev();
        let mut has_return = false; 
        for instr in function_instructions_rev_iter {
           if instr == &Instruction::from_operation(Operations::OpReturn){
                has_return = true;
                break;
            }
        }
        if !has_return {
            assert!(self.function_stack.last().unwrap().return_type == ValType::ValVoidType, "did not find a return statement for this non void returning funtion");
            //TODO: should this be a unique valdata enum value?
            self.emit_constant_from_val(Value::new(ValType::ValVoidType, ValData::Void));
        }
        

        let mut function = self.function_stack.pop().unwrap();
        // set the params
        function.set_params(params);
        // return the function
        function
    }

    fn parse_function_params(&mut self) -> Vec<ValType> {
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

    fn compile_argument(&mut self, token: &Token) -> ValType {
        // resolve the constant as a literal or local variable
        // call get_local or OP_CONSTANT to ensure that the arg is at the top of the stack
        // return the ValType of the arg value
        self.assert_is_constant(token);
        // if this is a variable, get local, else emit constant
        if token == &Token::TkIdentifier {
            let ident = self.parser.get_curr_slice();
            let idx = self.has_variable(ident).unwrap();
            self.emit_get_local(idx);
            self.current_chunk_ref().constants[idx].val_type
        } else {
            self.emit_constant_from_token(token);
            self.current_chunk_ref().constants.last().unwrap().val_type
        }
    }

    pub fn function_call(&mut self, function_idx: usize) -> ValType {
        // at the time of calling, the function will be at the top of the stack
        let mut return_type: ValType = ValType::ValVoidType;
        // consume the open paren
        self.parser.parse_next();
        let mut argument_count = 0;
        if !match_token(self.parser.peek(), Token::TkCloseParen) {
            let params;
            // I think TECHNICALLY the most efficient way to do this is to have parameters on
            // ObjFunction be an Rc<[ValType]> and then just clone the rc instead of copying over the
            // data with .to_owned().
            if let ValData::ValObj(rc_obj) = &self.current_chunk_ref().constants[function_idx].data
            {
                params = rc_obj.get_parameters().to_owned();
                return_type = rc_obj.get_return_type();
            } else {
                panic!("expected a function object");
            }

            // loop through the provided arguments, checking types and length
            loop {
                let param_type = params[argument_count];
                if let Some(argument_token) = self.parser.parse_next() {
                    let arg_type = self.compile_argument(&argument_token);
                    assert!(
                        param_type == arg_type,
                        "argument type {:?} does not match expected type {:?}",
                        arg_type,
                        param_type
                    );
                    argument_count += 1;
                    if match_token(self.parser.peek(), Token::TkCloseParen) {
                        // consume the close paren
                        self.parser.parse_next();
                        assert!(params.len() == argument_count);
                        break;
                    } else {
                        assert!(arg_type == param_type);
                        // consume the comma
                        self.parser.parse_next();
                    }
                }
            }
        } else {
            // this is a close paren, consume it.
            println!("THIs is a close paren!!!!! {:?}", self.parser.peek());
            self.parser.parse_next();
        }
        self.emit_call((argument_count + 1) as u8);

        
        return_type
    }

    pub fn expression_statement(&mut self) {
        self.expression(Token::TkSemicolon);
        // for set expression, we already the stack to move the value of the expression into the
        // local slot
        let idx = self.current_chunk_ref().code.len() - 2;
        if self.current_chunk_mut().code[idx] == Instruction::from_operation(Operations::OpSetLocal)
        {
            return;
        }

        //otherwise, pop the result manually
        self.emit_operation(Operations::OpPop);
    }

    pub fn assignment(&mut self) -> ValType {
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
}
