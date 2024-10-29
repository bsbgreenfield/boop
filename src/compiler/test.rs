use crate::compiler::*;
use Operations::*;
use std::fmt::Write;

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
        let code: &String = &String::from("1 + 2 * 3 * 4");
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
                Instruction::from_operation(OpConstant),
                Instruction::from_constant_idx(3),
                Instruction::from_operation(OpMultiply),
                Instruction::from_operation(OpAdd),
            ]
        );

        assert_eq!(
            &compiler.current_chunk_ref().constants,
            &vec![Value::from_num(1), Value::from_num(2), Value::from_num(3), Value::from_num(4)]
        );
    }

#[test]
fn compile_complex_1(){
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
