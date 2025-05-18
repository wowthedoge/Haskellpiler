module Parser where

import           Lexer
import           Text.Read (readMaybe)

data Expr
  = Number Double
  | Var String
  | Assign String Expr
  | BinOp Operator Expr Expr
  | Print String
  deriving (Show, Eq)

data Operator
  = OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  deriving (Show, Eq)

tokPrecedence :: Token -> Int
tokPrecedence TokPlus   = 1
tokPrecedence TokMinus  = 1
tokPrecedence TokTimes  = 2
tokPrecedence TokDivide = 2
tokPrecedence _         = 0

tokOperator :: Token -> Operator
tokOperator TokPlus   = OpPlus
tokOperator TokMinus  = OpMinus
tokOperator TokTimes  = OpTimes
tokOperator TokDivide = OpDivide
tokOperator _         = error "Invalid operator"

parser :: [Token] -> Expr
parser tokens =
  let (expr, rest) = parseExpr 0 tokens
   in case rest of
        [] -> expr
        _  -> error "Invalid expression"

parseNumber :: [Token] -> (Expr, [Token])
parseNumber (TokNumber n:tokens)
  -- trace ("parseNumber: " ++ show n)
 = (Number n, tokens)
parseNumber _ = error "Failed parsing number"

parseExpr :: Int -> [Token] -> (Expr, [Token])
parseExpr precedence (TokIdentifier name:rest) = (Var name, rest)
parseExpr precedence (TokVar:TokIdentifier name:TokAssign:rest) =
  let (rhs, rest') = parseExpr 0 rest
   in (Assign name rhs, rest')
parseExpr precedence (TokPrint:TokIdentifier name:rest) = (Print name, rest)
parseExpr precedence tokens
  -- trace
  --   ("parseExpr called with precedence "
  --      ++ show precedence`
  --      ++ ", tokens: "
  --      ++ show tokens)
  --   $
 =
  let (left, rest) = parseNumber tokens
   in parseInfix precedence left rest

parseInfix :: Int -> Expr -> [Token] -> (Expr, [Token])
parseInfix precedence left (op:rest)
  | tokPrecedence op > precedence
    -- trace ("parseInfix | higher prec token " ++ show op)
   =
    let (right, rest') = parseExpr (tokPrecedence op) rest
     in parseInfix precedence (BinOp (tokOperator op) left right) rest'
  | otherwise
    -- trace ("parseInfix | same/lower prec token " ++ show op)
   = (left, op : rest)
parseInfix _ left [] = (left, [])
