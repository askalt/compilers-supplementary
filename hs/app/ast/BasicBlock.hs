{-# LANGUAGE NamedFieldPuns #-}

module BasicBlock where

import Control.Applicative (asum)
import qualified Insn

-- Formatting indent.
indent :: String
indent = "   "

data BasicBlock = BasicBlock {label :: String, insns :: [Insn.Insn]}

instance Show BasicBlock where
  show BasicBlock {label, insns} = label ++ ":\n" ++ asum (map (\x -> indent ++ show x ++ ";\n") insns)
