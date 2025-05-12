module Main
  ( main
  ) where

import           Data.Char   (isDigit, isSpace)
import           Debug.Trace (trace)
import           Text.Read   (readMaybe)

main :: IO ()
main = receiveInput

receiveInput :: IO ()
receiveInput = do
  putStr "> "
  input <- getLine
  print (lexer input)
  print (parser (lexer input))
  print (eval (parser (lexer input)))
  receiveInput

eval :: Expr -> Double
eval (Number x)            = x
eval (BinOp op left right) = calc (eval left) op (eval right)

calc :: Double -> Operator -> Double -> Double
calc x op y =
  case op of
    Plus   -> x + y
    Minus  -> x - y
    Times  -> x * y
    Divide -> x / y

-- Lexer
data Token
  = TokNumber Double
  | TokPlus
  | TokMinus
  | TokTimes
  | TokDivide
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)About
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

-- Parser
data Expr
  = Number Double
  | BinOp Operator Expr Expr
  deriving (Show, Eq)

data Operator
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Show, Eq)

tokPrecedence :: Token -> Int
tokPrecedence TokPlus   = 1
tokPrecedence TokMinus  = 1
tokPrecedence TokTimes  = 2
tokPrecedence TokDivide = 2
tokPrecedence _         = 0

tokOperator :: Token -> Operator
tokOperator TokPlus   = Plus
tokOperator TokMinus  = Minus
tokOperator TokTimes  = Times
tokOperator TokDivide = Divide
tokOperator _         = error "Invalid operator"

parser :: [Token] -> Expr
parser tokens =
  let (expr, rest) = parseExpr 0 tokens
   in case rest of
        [] -> expr
        _  -> error "Invalid expression"  

parseNumber :: [Token] -> (Expr, [Token])
parseNumber (TokNumber n:tokens) =
  trace ("parseNumber: " ++ show n) (Number n, tokens)
parseNumber _ = error "Failed parsing number"

parseExpr :: Int -> [Token] -> (Expr, [Token])
parseExpr precedence tokens =
  trace
    ("parseExpr called with precedence "
       ++ show precedence
       ++ ", tokens: "
       ++ show tokens)
    $ let (left, rest) = parseNumber tokens
       in parseInfix precedence left rest

parseInfix :: Int -> Expr -> [Token] -> (Expr, [Token])
parseInfix precedence left (op:rest)
  | tokPrecedence op > precedence =
    trace ("parseInfix | higher prec token " ++ show op)
      $ let (right, rest') = parseExpr (tokPrecedence op) rest
         in parseInfix precedence (BinOp (tokOperator op) left right) rest'
  | otherwise =
    trace ("parseInfix | same/lower prec token " ++ show op) (left, op : rest)
parseInfix _ left [] = (left, [])
