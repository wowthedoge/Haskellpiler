module Main
  ( main
  ) where

import           Compiler
import           Debug.Trace    (trace)
import           Lexer
import           Parser
import           System.Process (callCommand)

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
  let bytecode = compiler ast
  print bytecode

  writeFile "bytecode.txt" (unlines (map show bytecode))
  
  callCommand "gcc -o interpreter interpreter.c"
  callCommand "./interpreter"

  receiveInput
