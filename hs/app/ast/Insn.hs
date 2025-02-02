{-# LANGUAGE NamedFieldPuns #-}

module Insn where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Type
import Value (FCall, RValue, Value, Variable)

newtype GotoInsn = GotoInsn {label :: String}

instance Show GotoInsn where
  show GotoInsn {label} = "goto " ++ label

data Insn
  = Assign {lhs :: Variable, rhs :: RValue}
  | AssignDeref {lhs :: Variable, drhs :: Value}
  | Goto GotoInsn
  | If {condition :: Value, thn :: GotoInsn, oth :: Maybe GotoInsn}
  | Return {ret :: Value}
  | Call {call :: FCall}
  | Declaration {typ :: Type, lhs :: Variable, declRhs :: Maybe RValue}

instance Show Insn where
  show insn = case insn of
    Assign {lhs, rhs} -> show lhs ++ " = " ++ show rhs
    AssignDeref {lhs, drhs} -> "*" ++ show lhs ++ " = " ++ show drhs
    Goto goto -> show goto
    If {condition, thn, oth} ->
      "if ("
        ++ show condition
        ++ ") "
        ++ show thn
        ++ fromJust (fmap (\x -> "; else " ++ show x) oth <|> Just "")
    Return {ret} -> "return " ++ show ret
    Call {call} -> show call
    Declaration {typ, lhs, declRhs} ->
      show typ ++ " " ++ show lhs ++ case declRhs of
        Just rhs -> " = " ++ show rhs
        Nothing -> ""
