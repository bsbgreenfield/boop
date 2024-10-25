mod compiler_main;
mod expression;
mod compiler_functions;
mod test;
use crate::parser::*;
use crate::value::*;
use crate::object::*;
use core::fmt;
use std::collections::hash_set::HashSet;

pub struct Compiler<'a> {
    parser: Parser<'a>,
    pub function_stack: Vec<ObjFunction>,
    types: HashSet<String>,
    scope_depth: u8,
}


#[derive(Debug, PartialEq, Clone, Copy)]
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
pub use compiler_main::*;
pub use expression::*;
pub use compiler_functions::*;

