use core::panic;

use crate::compiler::*;
impl<'a> Compiler<'a> {
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
                        let constant_idx = self.compile_operand(&first_token, operand_type_stack);
                        // check for the end of the expression.
                        if match_token(self.parser.peek(), expected_end_token) {
                            println!("expression has ended");
                            ExprResult::Done(*operand_type_stack.first().unwrap())
                        } else if match_token(self.parser.peek(), Token::TkOpenParen) {
                            let return_type = self.function_call(constant_idx);
                            operand_type_stack.pop();
                            operand_type_stack.push(return_type);
                            if match_token(self.parser.peek(), expected_end_token) {
                                println!("got to the end of call!!!!");
                                ExprResult::Done(return_type)
                            } else {
                                ExprResult::ParsedOperand
                            }
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
                let did_close = self.dump_group(operator_stack, operand_type_stack);
                // we were expecting a parentheses to end the expression
                if expected_end_token == Token::TkCloseParen {
                    // unclosed group end indicates end of expression
                    if !did_close {
                        self.dump_all(operator_stack, operand_type_stack);
                        assert_eq!(operand_type_stack.len(), 1);
                        assert_eq!(operator_stack.len(), 0);
                        return Some(ExprResult::Done(*operand_type_stack.first().unwrap()));
                    }
                } else if !did_close {
                    panic!("unmatched close parentheses");
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
                    // check if the token is a group start
                    if is_group_start(&token) {
                        operator_stack.push(Operations::OpGrouping);
                        continue;
                    }

                    if operand_phase {
                        // prefix unary
                        if token == Token::TkBang {
                            operator_stack.push(Operations::OpNot);
                            continue;
                        }

                        // compile operand
                        let constant_idx = self.compile_operand(&token, &mut operand_type_stack);
                        operand_phase = false;

                        // postfix
                        if let Some(postfix) = self.parser.peek() {
                            if postfix == Token::TkOpenParen {
                                let return_type = self.function_call(constant_idx);
                                // replace the ObjFunction with the return type of the function
                                operand_type_stack.pop();
                                operand_type_stack.push(return_type);
                                // TODO: when this is called in the vm, we have to remember to pop
                                // the function AND all its arguments OFF the stack, and replace it
                                // with the return value of the function
                            }
                        }

                        // check for group end
                        if let Some(result) = self.expr_check_group_end(
                            &mut operator_stack,
                            &mut operand_type_stack,
                            expected_end_token,
                        ) {
                            match result {
                                ExprResult::Done(val_type) => return val_type,
                                _ => panic!("unexpected result"),
                            }
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
                            self.dump_operators(
                                &mut operator_stack,
                                &mut operand_type_stack,
                                operator,
                            );
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

    pub fn assert_is_constant(&self, token: &Token) {
        use Token::*;
        assert!(matches!(
            token,
            TkNum | TkString | TkTrue | TkFalse | TkIdentifier
        ));
    }

    pub fn compile_operand(
        &mut self,
        token: &Token,
        operand_type_stack: &mut Vec<ValType>,
    ) -> usize {
        self.assert_is_constant(token);
        // if this is a variable, get local, else emit constant
        if token == &Token::TkIdentifier {
            let ident = self.parser.get_curr_slice();
            let idx = self.has_variable(ident).expect("invalid variable name");
            self.emit_get_local(idx);
            push_type(
                self.current_chunk_ref().locals[idx].val_type,
                operand_type_stack,
            );
            idx
        } else {
            self.emit_constant_from_token(token);
            let new_val_ref = self.current_chunk_ref().constants.last().unwrap();
            push_type_of_val(new_val_ref, operand_type_stack);
            self.current_chunk_ref().constants.len()
        }
    }

    fn dump_all(&mut self, stack: &mut Vec<Operations>, operand_type_stack: &mut Vec<ValType>) {
        loop {
            if self.dump_stack(stack, operand_type_stack).is_some() {
                continue;
            } else {
                break;
            }
        }
    }
    fn dump_group(
        &mut self,
        stack: &mut Vec<Operations>,
        operand_type_stack: &mut Vec<ValType>,
    ) -> bool {
        // dump operators one by one until we hit the OP_GROUP operation
        // if this is never found, return false to indicate an unmatched paren
        loop {
            if let Some(popped_operation) = self.dump_stack(stack, operand_type_stack) {
                if popped_operation == Operations::OpGrouping {
                    return true;
                }
            } else {
                return false;
            }
        }
    }
    fn dump_operators(
        &mut self,
        stack: &mut Vec<Operations>,
        operand_type_stack: &mut Vec<ValType>,
        incoming_operator: Operations,
    ) -> bool {
        // dump operators until we hit an operator that is of a lower precedence than the incoming
        // true indicidates the stack was entirely dumped
        // false indicates lower precedence operator caused the function to end
        loop {
            if self.dump_stack(stack, operand_type_stack).is_some() {
                if prec_of(top_of(stack).unwrap_or(&Operations::NoOp)) < prec_of(&incoming_operator)
                {
                    stack.push(incoming_operator);
                    return false;
                }
            } else {
                stack.push(incoming_operator);
                return true;
            }
        }
    }

    fn dump_stack(
        &mut self,
        stack: &mut Vec<Operations>,
        operand_type_stack: &mut Vec<ValType>,
    ) -> Option<Operations> {
        if stack.is_empty() {
            return None;
        }
        if let Some(operation) = stack.pop() {
            match operation {
                Operations::OpGrouping => {}
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
                    // Binary operators
                    // pop the top two operands, validate that these two are able to perform the
                    // requested operation, then push the correct val type back onto the stack
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
                            if can_multiply_or_divide(operand_1, operand_2, operand_type_stack) {
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
                        | Operations::OpCall
                        | Operations::OpReturn
                        | Operations::OpJumpIfFalse => panic!("expected an operator operation! got {:?}", operation)
                    }
                }
            }
            Some(operation)
        } else {
            None
        }
    }
}
