/// Operation that return bool, but maybe
/// operate with other types.
///
/// Examples:
///  
/// a == 3
///   ^
///  [`LogicalOp`]
///
/// a & b
///   ^
///  [`LogicalOp`]
///
pub enum LogicalOp {
    Eq,
    Gq,
    Lq,
    Gt,
    Lt,
    And,
    Xor,
    Or,
    Not,
}

/// Arithmetical operation.
///
/// Example:
///
/// a + b
///   ^
///  [`ArithmOp`]
///
pub enum ArithmOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Operation with bits.
///
/// Example:
///
/// a >> 3
///   ^
/// [`BitOp`]
///
pub enum BitOp {
    Flip,
    Shl,
    Shr,
}

/// Represents the set of binary operations.
pub enum BinaryOp {
    LogicalOp(LogicalOp),
    ArithmOp(ArithmOp),
    BitOp(BitOp),
}

/// Represents the set of unary operations.
pub enum UnaryOp {
    Plus,
    Minus,
    Star,
    Ref,
    Not,
}

/// Represents the set of supported types.
pub enum Type {
    Int32,
    Unsigned64,
    Bool,
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
/// int x = a    +   3;
///         ^        ^
///        value    value
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

/// Represents a `goto` instruction.
pub struct GotoInsn {
    /// Label where we go.
    pub label: String,
}

/// Represents an instruction in 3AC form.
/// See https://en.wikipedia.org/wiki/Three-address_code
pub enum Insn {
    /// Variable assignment.
    Assign { lhs: Variable, rhs: RValue },
    /// Dereference assignment.
    AssignDeref { lhs: Variable, rhs: Value },
    /// Transition.
    Goto(GotoInsn),
    /// Condition.
    ///
    /// if (a) {
    ///     goto l1;
    /// } else {
    ///     goto l2;
    /// }
    ///
    If {
        condition: Value,
        on_true: GotoInsn,
        on_false: GotoInsn,
    },
    /// Return.
    Return(Value),
}

/// Represents a basic block.
pub struct BasicBlock {
    /// Label.
    pub label: String,
    /// Instructions.
    pub insns: Vec<Insn>,
}

/// Represents a function.
pub struct Function {
    /// Return type.
    pub return_type: Type,
    /// Arguments.
    pub args: Vec<Variable>,
    /// Top level variables declaration.
    pub decl: Vec<Variable>,
    /// Basic blocks.
    pub blocks: Vec<BasicBlock>,
}

/// Module ast.
pub type Ast = Vec<Function>;
