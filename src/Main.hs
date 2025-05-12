module Main
  ( main
  ) where

import           Compiler
import           Debug.Trace (trace)
import           Lexer
import           Parser

main :: IO ()
main = receiveInput

receiveInput :: IO ()
receiveInput = do
  putStr "> "
  input <- getLine
  let tokens = lexer input
  print tokens
  let ast = parser tokens
  print ast
  print (eval ast)
  let bytecode = compiler ast
  print bytecode
  receiveInput

eval :: Expr -> Double
eval (Number x)            = x
eval (BinOp op left right) = calc (eval left) op (eval right)

calc :: Double -> Operator -> Double -> Double
calc x op y =
  case op of
    OpPlus   -> x + y
    OpMinus  -> x - y
    OpTimes  -> x * y
    OpDivide -> x / y
