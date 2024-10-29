use crate::compiler::*;

pub fn prec_of(operation: &Operations) -> Precedence {
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
        OpCall => PrecCall,
        OpReturn => PrecNone
    }
}

pub fn top_of<T>(stack: &[T]) -> Option<&T> {
    let length = stack.len();
    if length == 0 {
        None
    } else {
        Some(&stack[length - 1])
    }
}

pub fn is_group_start(token: &Token) -> bool {
    matches!(token, Token::TkOpenParen)
}


pub fn can_add_or_subtract(
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

pub fn can_multiply_or_divide(
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

pub fn can_and_or_or(
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

pub fn can_compare(
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

pub fn push_type_of_val(val: &Value, operand_type_stack: &mut Vec<ValType>) {
    operand_type_stack.push(val.val_type);
}

pub fn push_type(val_type: ValType, operand_type_stack: &mut Vec<ValType>) {
    operand_type_stack.push(val_type);
}

pub fn match_token(maybe_token: Option<Token>, expected: Token) -> bool {
    if let Some(token) = maybe_token {
        return token == expected;
    }
    false
}

pub fn match_val_type(val_ident: &str) -> ValType {
    match val_ident {
        "int" => ValType::ValNumType,
        "String" => ValType::ValStringType,
        "bool" => ValType::ValBoolType,
        "void" => ValType::ValVoidType,
        _ => panic!("havent implemented this type!!!"),
    }
}
