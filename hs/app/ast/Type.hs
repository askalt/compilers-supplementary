module Type where

data Type
  = Int32
  | Unsigned32
  | Bool
  | Void
  | Ref Type

instance Show Type where
  show op = case op of
    Int32 -> "int"
    Unsigned32 -> "unsigned"
    Bool -> "bool"
    Void -> "void"
    Ref t -> show t ++ "*"
