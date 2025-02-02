{-# LANGUAGE NamedFieldPuns #-}

module Value where

import Data.List (intercalate)
import Op (BinaryOp, UnaryOp)

newtype Variable = Variable {name :: String}

instance Show Variable where
  show Variable {name} = name

newtype Literal = Literal {val :: Int}

instance Show Literal where
  show Literal {val} = show val

data Value = ValueVariable {var :: Variable} | ValueLiteral {lit :: Literal}

instance Show Value where
  show v = case v of
    ValueVariable {var} -> show var
    ValueLiteral {lit} -> show lit

data FCall = FCall {fun :: String, args :: [Value]}

instance Show FCall where
  show FCall {fun, args} = fun ++ "(" ++ intercalate "," (map show args) ++ ")"

data RValue
  = Binary {lhs :: Value, bop :: BinaryOp, rhs :: Value}
  | Unary {uop :: UnaryOp, arg :: Value}
  | Val Value
  | Call FCall

instance Show RValue where
  show op = case op of
    Binary {lhs, bop, rhs} -> show lhs ++ " " ++ show bop ++ " " ++ show rhs
    Unary {uop, arg} -> show uop ++ show arg
    Val val -> show val
    Call call -> show call
