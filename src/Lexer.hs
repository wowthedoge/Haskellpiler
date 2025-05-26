module Lexer where

import           Data.Char (isAlpha, isDigit, isSpace)
import           Text.Read (readMaybe)

data Token
  = TokNumber Double
  | TokPlus
  | TokMinus
  | TokTimes
  | TokDivide
  | TokLParen
  | TokRParen
  | TokVar
  | TokIdentifier String
  | TokAssign
  | TokPrint
  | TokFunc
  | TokComma
  | TokLBrace
  | TokRBrace
  | TokReturn
  | TokSemicolon
  deriving (Show, Eq)

keywords :: [(String, Token)]
keywords =
  [ ("var", TokVar)
  , ("print", TokPrint)
  , ("func", TokFunc)
  , ("return", TokReturn)
  ]

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isDigit c = lexNumber (c : cs)
  | isAlpha c = lexIdentifier (c : cs)
  | c == '+' = TokPlus : lexer cs
  | c == '-' = TokMinus : lexer cs
  | c == '*' = TokTimes : lexer cs
  | c == '/' = TokDivide : lexer cs
  | c == '=' = TokAssign : lexer cs
  | c == '(' = TokLParen : lexer cs
  | c == ')' = TokRParen : lexer cs
  | c == ',' = TokComma : lexer cs
  | c == '{' = TokLBrace : lexer cs
  | c == '}' = TokRBrace : lexer cs
  | c == ';' = TokSemicolon : lexer cs
  | otherwise = error ("Invalid character: " ++ [c])

lexNumber :: String -> [Token]
lexNumber cs =
  let (numStr, rest) = span (\c -> isDigit c || c == '.') cs
   in case readMaybe numStr of
        Just num -> TokNumber num : lexer rest
        Nothing  -> error ("Invalid number: " ++ numStr)

lexIdentifier :: String -> [Token]
lexIdentifier cs =
  let (idStr, rest) = span (\c -> isAlpha c || isDigit c) cs
   in case lookup idStr keywords of
        Just keyword -> keyword : lexer rest
        Nothing      -> TokIdentifier idStr : lexer rest
