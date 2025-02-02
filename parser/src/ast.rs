use derive_more::derive::Display;

// Formatting indent.
const INDENT: &str = "   ";

/// Operation that return bool, but maybe
/// operate with other types.
///
/// Examples:
///
/// """
/// a == 3
///   ^
///  [`LogicalOp`]
/// """
///
/// """
/// a & b
///   ^
///  [`LogicalOp`]
/// """
///
#[derive(Debug, Display)]
pub enum LogicalBinarylOp {
    #[display("==")]
    Eq,
    #[display(">=")]
    Gq,
    #[display("<=")]
    Lq,
    #[display(">")]
    Gt,
    #[display("<")]
    Lt,
    #[display("&")]
    And,
    #[display("^")]
    Xor,
    #[display("|")]
    Or,
}

/// Arithmetical operation.
///
/// Example:
///
/// """
/// a + b
///   ^
///  [`ArithmOp`]
/// """
///
#[derive(Debug, Display)]
pub enum ArithmBinaryOp {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,
    #[display("%")]
    Mod,
}

/// Operation with bits.
///
/// Example:
///
/// """
/// a >> 3
///   ^
/// [`BitOp`]
/// """
///
#[derive(Debug, Display)]
pub enum BitBinaryOp {
    #[display("~")]
    Flip,
    #[display("<<")]
    Shl,
    #[display(">>")]
    Shr,
}

/// Represents the set of binary operations.
#[derive(Debug, Display)]
pub enum BinaryOp {
    LogicalOp(LogicalBinarylOp),
    ArithmOp(ArithmBinaryOp),
    BitOp(BitBinaryOp),
}

/// Represents the set of unary operations.
#[derive(Debug, Display)]
pub enum UnaryOp {
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Deref,
    #[display("&")]
    Ref,
    #[display("!")]
    Not,
    #[display("~")]
    Neg,
}

/// Represents the set of supported types.
#[derive(Debug, Clone, Display)]
pub enum Type {
    #[display("int")]
    Int32,
    #[display("unsigned")]
    Unsigned32,
    #[display("bool")]
    Bool,
    #[display("void")]
    Void,
    #[display("*{}", _0)]
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
#[derive(Debug, Display)]
pub struct Variable {
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
#[derive(Debug, Display)]
pub struct Literal {
    pub val: i64,
}

/// Represents a value that is used in contexts:
///
/// """
/// int x = a    +   3;
///         ^        ^
///        value    value
/// """
///
/// """
/// int x = -3;
///         ^
///        value
/// """
///
/// """
/// return 3;
///        ^
///       value
/// """
///
/// """
/// return a;
///        ^
///      value
/// """
///
#[derive(Debug, Display)]
pub enum Value {
    Variable(Variable),
    Literal(Literal),
}

/// Temporary calculation, literal, or memory cell that
/// could be present at the right in assign where left
/// operand is no dereference.
///
#[derive(Debug, Display)]
pub enum RValue {
    #[display("{} {} {}", lhs, op, rhs)]
    BinaryOp {
        lhs: Value,
        op: BinaryOp,
        rhs: Value,
    },
    #[display("{}{}", op, arg)]
    UnaryOp {
        op: UnaryOp,
        arg: Value,
    },
    Value(Value),
    Call(Call),
}

/// Represents a `goto` instruction.
#[derive(Debug, Display)]
#[display("goto {}", label)]
pub struct GotoInsn {
    /// Label where we go.
    pub label: String,
}

/// Represents a function call.
#[derive(Debug, Display)]
#[display("{}({})", fun,
    args.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(","))]
pub struct Call {
    pub fun: String,
    pub args: Vec<Value>,
}

/// Represents an instruction in 3AC form.
/// See https://en.wikipedia.org/wiki/Three-address_code
#[derive(Debug, Display)]
pub enum Insn {
    /// Variable assignment.
    #[display("{} = {}", lhs, rhs)]
    Assign { lhs: Variable, rhs: RValue },
    /// Dereference assignment.
    #[display("*{} = {}", lhs, rhs)]
    AssignDeref { lhs: Variable, rhs: Value },
    /// Transition.
    Goto(GotoInsn),
    /// Condition.
    ///
    /// """
    /// if (a)
    ///     goto l1;
    /// else
    ///     goto l2;
    /// """
    #[display("if ({}) {}{}", condition, then,
        otherwise.as_ref().map(|e| format!("; else {}", e)).unwrap_or("".to_owned()))]
    If {
        condition: Value,
        then: GotoInsn,
        otherwise: Option<GotoInsn>,
    },
    #[display("return {}", _0)]
    /// Return.
    Return(Value),
    /// Function call.
    Call(Call),
    #[display("{} {}{}", typ, lhs,
        rhs.as_ref().map(
            |rhs| format!(" = {}", rhs)
        ).unwrap_or("".to_owned())
    )]
    Declaration {
        typ: Type,
        lhs: Variable,
        rhs: Option<RValue>,
    },
}

impl Insn {
    pub fn into_goto(self) -> Option<GotoInsn> {
        match self {
            Self::Goto(goto) => Some(goto),
            _ => None,
        }
    }
}

/// Represents a basic block.
#[derive(Debug, Display)]
#[display("{}:\n{};",
    label,
    insns.iter().map(|e| format!("{}{}", INDENT, e)).collect::<Vec<_>>().join(";\n")
)]
pub struct BasicBlock {
    /// Label.
    pub label: String,
    /// Instructions.
    pub insns: Vec<Insn>,
}

/// Represents a function.
#[derive(Debug, Display)]
#[display("{} {}({}) {{\n{}\n}}\n",
    return_type, name,
    args.iter().map(
        |(typ, name)| format!("{} {}", typ, name,
    )).collect::<Vec<_>>().join(","),
    blocks.iter().map(|b| format!("{}", b)).collect::<Vec<_>>().join("\n\n")
)]
pub struct Function {
    /// Return type.
    pub return_type: Type,
    /// Name,
    pub name: String,
    /// Arguments.
    pub args: Vec<(Type, Variable)>,
    /// Basic blocks.
    pub blocks: Vec<BasicBlock>,
}

/// Module ast.
///
#[derive(Display)]
#[display("{}", funs.iter().map(|f| format!("{}", f)).collect::<Vec<_>>().join("\n\n") )]
pub struct Ast {
    pub funs: Vec<Function>,
}
