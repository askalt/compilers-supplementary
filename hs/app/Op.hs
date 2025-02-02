module Op where

data LogicalBinaryOp
  = Eq
  | Gq
  | Lq
  | Gt
  | Lt
  | And
  | Xor
  | Or

instance Show LogicalBinaryOp where
  show op = case op of
    Eq -> "=="
    Gq -> ">="
    Lq -> "<="
    Gt -> ">"
    Lt -> "<"
    And -> "&"
    Xor -> "^"
    Or -> "|"

data ArithmBinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod

instance Show ArithmBinaryOp where
  show op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"

data BitBinaryOp
  = Flip
  | Shl
  | Shr

instance Show BitBinaryOp where
  show op = case op of
    Flip -> "~"
    Shl -> "<<"
    Shr -> ">>"

data UnaryOp
  = Plus
  | Minus
  | Deref
  | RefOf
  | Not
  | Neg

instance Show UnaryOp where
  show op = case op of
    Plus -> "+"
    Minus -> "-"
    Deref -> "*"
    RefOf -> "&"
    Not -> "!"
    Neg -> "~"

data BinaryOp = Logical LogicalBinaryOp | Arithm ArithmBinaryOp | Bit BitBinaryOp

instance Show BinaryOp where
  show op = case op of
    Logical l -> show l
    Arithm a -> show a
    Bit b -> show b
