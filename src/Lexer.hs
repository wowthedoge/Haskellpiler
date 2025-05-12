module Lexer where

import           Data.Char (isDigit, isSpace)
import           Text.Read (readMaybe)


data Token
  = TokNumber Double
  | TokPlus
  | TokMinus
  | TokTimes
  | TokDivide
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isDigit c = lexNumber (c : cs)
  | c == '+' = TokPlus : lexer cs
  | c == '-' = TokMinus : lexer cs
  | c == '*' = TokTimes : lexer cs
  | c == '/' = TokDivide : lexer cs
  | otherwise = error ("Invalid character: " ++ [c])

lexNumber :: String -> [Token]
lexNumber cs =
  let (numStr, rest) = span (\c -> isDigit c || c == '.') cs
   in case readMaybe numStr of
        Just num -> TokNumber num : lexer rest
        Nothing  -> error ("Invalid number: " ++ numStr)
