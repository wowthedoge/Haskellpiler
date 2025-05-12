module Compiler where

import           Parser

data Bytecode
  = Push Double
  | Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show, Eq)

compiler :: Expr -> [Bytecode]
compiler (Number x) = [Push x]
compiler (BinOp op left right) =
  compiler left ++ compiler right ++ [opToBytecode op]
  where
    opToBytecode :: Operator -> Bytecode
    opToBytecode OpPlus   = Add
    opToBytecode OpMinus  = Subtract
    opToBytecode OpTimes  = Multiply
    opToBytecode OpDivide = Divide
