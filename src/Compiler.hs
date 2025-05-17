module Compiler where

import           Parser

data Bytecode
  = PUSH Double
  | ADD
  | SUB
  | MUL
  | DIV
  | LOAD String
  | STORE String
  deriving (Show, Eq)

compiler :: Expr -> [Bytecode]
compiler (Number x) = [PUSH x]
compiler (Var name) = [LOAD name]
compiler (Assign name expr) =
  compiler expr ++ [STORE name]
compiler (BinOp op left right) =
  compiler left ++ compiler right ++ [opToBytecode op]
  where
    opToBytecode :: Operator -> Bytecode
    opToBytecode OpPlus   = ADD
    opToBytecode OpMinus  = SUB
    opToBytecode OpTimes  = MUL
    opToBytecode OpDivide = DIV
