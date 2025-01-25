/// Represents the set of builtin binary ops.
pub enum BinaryOp {
    Eq,
    Gq,
    Lq,
    Gt,
    Lt,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    And,
    Xor,
    Or,
    Not,

    Shl,
    Shr,
}

/// Represents the set of builtin unary op.
pub enum UnaryOp {
    Plus,
    Minus,
    Star,
    Ref,
    Not,
}

/// Represents the set of types.
pub enum Type {
    Int32,
    Unsigned64,
    Ref(Box<Type>),
}

/// Represents a named typed memory cell.
///
/// Example:
///
/// """
/// int x = 3;
/// """
///
/// x is a variable.
///
pub struct Variable {
    pub typ: Type,
    pub name: String,
}

/// Represents a literal inside program code.
///
/// Example:
///
/// """
/// int x = 3;
/// """
///
/// 3 is a literal.
pub struct Literal {
    pub val: i64,
}

/// Represents a value that is used in contexts:
///
/// int x = a     +    3;
///         ^          ^
///        value       value
///
/// int x = -3;
///         ^
///        value
///
/// return 3;
///        ^
///       value
///
/// return a;
///        ^
///      value
///
pub enum Value {
    Variable(Variable),
    Literal(Literal),
}

/// Temporary calculation, literal, or memory cell that
/// could be present at the right in assign where left
/// operand is no dereference.
///
pub enum RValue {
    BinaryOp {
        lhs: Value,
        op: BinaryOp,
        rhs: Value,
    },
    UnaryOp {
        op: UnaryOp,
        arg: Value,
    },
    Value(Value),
}

//
// int foo(int ***a) {
//     int **b = *a,
//     int *c  = *b;
//     *c = 3;
//     return 42;
// }
//
// void assign_3(int *x) {
//    *x = 3;
// }
//
// int  x    = 3;
// int* addr = &x;
// assign_3(&x);
//
// int x = 3;
// return &x;
//

/// Represents instruction in 3AC form.
/// See https://en.wikipedia.org/wiki/Three-address_code
pub enum Insn {
    /// Variable assignment.
    Assign(Variable, RValue),
    /// Dereference assignment.
    AssignDeref(Variable, Value),
    /// Transition.
    Goto,
    If,
    Return(Value),
}

pub struct BasicBlock {
    pub name: String,
    pub insns: Vec<Insn>,
}

pub struct FunctionBody {
    pub decl: Vec<Variable>,
    pub blocks: Vec<BasicBlock>,
}

pub struct Function {
    pub return_type: Type,
    pub args: Vec<Variable>,
    pub body: FunctionBody,
}

pub type Ast = Vec<Function>;
