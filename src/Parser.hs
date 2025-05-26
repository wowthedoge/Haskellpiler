module Parser where

import           Data.List.Split (splitOn)
import           Debug.Trace     (trace)
import           Lexer
import           Text.Read       (readMaybe)

data Expr
  = Number Double
  | Var String
  | Assign String Expr
  | FuncDef String [String] [Expr]
  | Return Expr
  | BinOp Operator Expr Expr
  | Print String
  | Sequence [Expr]
  deriving (Show, Eq)

data Operator
  = OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  deriving (Show, Eq)

parser :: [Token] -> Expr
parser tokens =
  let (exprs, _) = parseSequence tokens
   in Sequence exprs

parseSequence :: [Token] -> ([Expr], [Token])
parseSequence [] = trace "parseSequence | End of tokens" ([], [])
parseSequence (TokRBrace:rest) =
  trace
    "parseSequence | Found closing brace, stopping sequence parsing"
    ([], TokRBrace : rest)
parseSequence tokens =
  trace ("parseSequence | Tokens: " ++ show tokens)
    $ let (expr, rest) = parseExpr tokens
       in trace ("parseSequence | Parsed expression: " ++ show expr)
            $ case rest of
                (TokSemicolon:rest') ->
                  trace
                    "parseSequence | Found semicolon, continuing to parse next expression"
                    $ let (exprs, finalRest) = parseSequence rest'
                       in trace
                            ("parseSequence | Adding expression to sequence: "
                               ++ show expr)
                            (expr : exprs, finalRest)
                _ ->
                  trace
                    ("parseSequence | No semicolon, returning single expression: "
                       ++ show expr)
                    ([expr], rest)

parseExpr :: [Token] -> (Expr, [Token])
-- Assign
parseExpr (TokVar:TokIdentifier name:TokAssign:rest) =
  trace ("parseExpr | Assign: " ++ name)
    $ let (rhs, rest') = parseExpr rest
       in (Assign name rhs, rest')
-- Functions
parseExpr (TokFunc:TokIdentifier name:TokLParen:rest) =
  let (args, rest') = trace "parseExpr | parseFunc" parseFuncArgs rest
   in case rest' of
        (TokRParen:TokLBrace:rest'') ->
          let (body, rest''') = parseFuncBody rest''
           in case body of
                []    -> (FuncDef name args [], rest''')
                exprs -> (FuncDef name args exprs, rest''')
        _ -> error "Missing closing parenthesis in function definition"
-- Return
parseExpr (TokReturn:rest) =
  trace "parseExpr | Return"
    $ let (expr, rest') = parseExpr rest
       in (Return expr, rest')
-- Print
parseExpr (TokPrint:TokIdentifier name:rest) = (Print name, rest)
-- BinOps
parseExpr tokens =
  trace ("parseExpr | parseBinOps: " ++ show tokens) $ parseBinOps 0 tokens

-- Helpers
parseFuncArgs :: [Token] -> ([String], [Token])
parseFuncArgs (TokIdentifier arg:TokComma:rest) =
  trace ("parseFuncArgs | arg: " ++ arg)
    $ let (args, rest') = parseFuncArgs rest
       in (arg : args, rest')
parseFuncArgs (TokIdentifier arg:rest) =
  trace ("parseFuncArgs | arg: " ++ arg) ([arg], rest)
parseFuncArgs tokens =
  error ("Invalid arguments in function definition: " ++ show tokens)

parseFuncBody :: [Token] -> ([Expr], [Token])
parseFuncBody tokens =
  let (exprs, rest) = parseSequence tokens
   in case rest of
        (TokRBrace:rest') -> (exprs, rest')
        _                 -> error "Missing closing brace for function body"

parseBinOps :: Int -> [Token] -> (Expr, [Token])
parseBinOps precedence tokens =
  let (left, rest) = parsePrimary tokens
   in parseInfix precedence left rest

parseInfix :: Int -> Expr -> [Token] -> (Expr, [Token])
parseInfix precedence left (op:rest)
  | isOperator op && tokPrecedence op >= precedence =
    let (right, rest') = parseBinOps (tokPrecedence op + 1) rest
     in parseInfix precedence (BinOp (tokOperator op) left right) rest'
  | otherwise = (left, op : rest)
parseInfix _ left [] = (left, [])

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (TokNumber n:rest) =
  trace ("parsePrimary | number: " ++ show n) (Number n, rest)
parsePrimary (TokIdentifier id:rest) =
  trace ("parsePrimary | identifier: " ++ id) (Var id, rest)
parsePrimary (TokLParen:rest) =
  trace "parsePrimary | parentheses"
    $ let (expr, rest') = parseExpr rest
       in case rest' of
            (TokRParen:rest'') -> (expr, rest'')
            _                  -> error "Missing closing parenthesis"
parsePrimary _ = error "Invalid token for expression"

isOperator :: Token -> Bool
isOperator TokPlus   = True
isOperator TokMinus  = True
isOperator TokTimes  = True
isOperator TokDivide = True
isOperator _         = False

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
