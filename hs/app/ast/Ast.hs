{-# LANGUAGE NamedFieldPuns #-}

module Ast where

import Data.List (intercalate)
import qualified Function

newtype Ast = Ast {funs :: [Function.Function]}

instance Show Ast where
  show Ast {funs} = intercalate "\n\n" (fmap show funs) ++ "\n\n"
