{-# LANGUAGE NamedFieldPuns #-}

module Function (Function (..)) where

import qualified BasicBlock
import Data.List (intercalate)
import qualified Type
import qualified Value

data Function = Function {return_type :: Type.Type, name :: String, args :: [(Type.Type, Value.Variable)], blocks :: [BasicBlock.BasicBlock]}

instance Show Function where
  show Function {return_type, name, args, blocks} = show return_type ++ " " ++ name ++ "(" ++ intercalate "," (map (\(typ, n) -> show typ ++ " " ++ show n) args) ++ ") {\n" ++ intercalate "\n" (map show blocks) ++ "}"
