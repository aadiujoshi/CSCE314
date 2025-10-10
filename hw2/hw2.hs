module HW_Expr
  ( Op(..)
  , Expr(..)
  , Token(..)
  , tokenize
  , parseExprString
  , run
  , showPrefix
  , toPrefix
  , eval
  ) where

-- CSCE 314 — HW: Parse & Evaluate Arithmetic (Prefix AST)
-- Starter file for students.
-- Replace each `undefined` with your implementation and remove TODOs as you complete them.
-- You may ONLY use Prelude and Data.Char (no external parsing libraries).

import Data.Char (isDigit, isSpace)

-- -------------------------------------------------------------------------
-- Operators and AST (Given)
-- -------------------------------------------------------------------------

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

data Expr
  = Lit Integer
  | Bin Op Expr Expr
  deriving (Eq, Show)

-- -------------------------------------------------------------------------
-- Tokens (Given)
-- -------------------------------------------------------------------------

data Token
  = TPlus | TMinus | TMul | TDiv
  | TLParen | TRParen
  | TInt Integer
  deriving (Eq, Show)

-- -------------------------------------------------------------------------
-- 1) TOKENIZER
--    TODO: Implement 'tokenize' that turns a String into [Token].
--    - Ignore whitespace
--    - Support multi-digit integers
--    - Recognize + - * / ( )
--    - You may use: isDigit, isSpace
--    - (Optional) You can choose to throw an error on unknown characters
--      or extend the assignment to return Either for tokenizer errors.
-- -------------------------------------------------------------------------

tokenize :: String -> [Token]
tokenize expression = parseTokens "" expression

-- PLAN
-- example: -5 + -(-10 * 3 / 1) - 4
-- if any operator besides minus, parse as token immediatly and reset
-- if minus, continue
-- if tracking is minus and cur is lparen, parse as int(-1) and lparen and reset 
-- if cur is rparen, parse as int and rparen and reset
-- if cur is space, we are guaranteed to have been tracking a valid number

--keep track of current continous string
--go char by char
--this impl accounts for double whitespace
--                 track    rem         res
parseTokens :: String -> String -> [Token]
parseTokens tracking ""
    | null tracking = []
    | otherwise     = [TInt (read tracking)]

parseTokens tracking (cur:remaining)
    | cur == '*' =                          TMul : parseTokens "" remaining
    | cur == '/' =                          TDiv : parseTokens "" remaining
    | cur == '+' =                          TPlus : parseTokens "" remaining
    | cur == '-' =                          parseTokens (tracking ++ [cur]) remaining
    | cur == '(' && tracking == "-" =       TInt (-1) : TMul : TLParen : parseTokens "" remaining
    | cur == '(' =                          TLParen : parseTokens "" remaining
    | cur == ')' && not (null tracking) =   TInt (read tracking) : TRParen : parseTokens "" remaining
    | cur == ')' =                          TRParen : parseTokens "" remaining
    | isSpace cur && tracking == "-" =      TMinus : parseTokens "" remaining
    | isSpace cur && not (null tracking) =  TInt (read tracking) : parseTokens "" remaining
    | isSpace cur =                         parseTokens "" remaining
    | otherwise =                           parseTokens (tracking ++ [cur]) remaining

-- -------------------------------------------------------------------------
-- 2) PARSER (Recursive Descent)
-- Grammar:
--   Expr   ::= Term (('+' | '-') Term)*
--   Term   ::= Factor (('*' | '/') Factor)*
--   Factor ::= INT | '(' Expr ')'
-- Return type pattern for nonterminals:
--   parseX :: [Token] -> Either String (Expr, [Token])
-- Notes:
--   - Use recursion
--   - Propagate errors via Left "message"
-- -------------------------------------------------------------------------

parseExprString :: String -> Either String Expr
parseExprString s = 
    -- let tokens =   -- First, convert string to tokens
    case parseExpr (tokenize s) of
        Left err -> Left err  -- Propagate parse error
        Right (expr, []) -> Right expr  -- Success: no tokens left
        Right (_, remaining) -> Left $ "Unexpected tokens at end: " ++ show remaining

-- HINT: define helpers (signatures given; bodies are TODO)
parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr = undefined -- TODO

parseTerm :: [Token] -> Either String (Expr, [Token])
parseTerm = undefined -- TODO

parseFactor :: [Token] -> Either String (Expr, [Token])
parseFactor = undefined  -- TODO

-- -------------------------------------------------------------------------
-- 3) PRETTY-PRINT (Prefix)
--   toPrefix (Lit 2) == "2"
--   toPrefix (Bin Add (Lit 2) (Lit 3)) == "add 2 3"
--   Wrap nested subexpressions in parentheses for readability:
--   toPrefix (Bin Add (Bin Mul (Lit 2) (Lit 5)) (Lit 9))
--     == "add (mult 2 5) 9"
-- -------------------------------------------------------------------------

toPrefix :: Expr -> String
toPrefix = undefined  -- TODO

-- -------------------------------------------------------------------------
-- 4) EVALUATION (Safe with Either)
--   eval (Lit 2) == Right 2
--   eval (Bin Div (Lit 8) (Lit 0)) == Left "Division by zero"
--   Use recursion and pattern matching
-- -------------------------------------------------------------------------

eval :: Expr -> Either String Integer
eval = undefined  -- TODO

-- -------------------------------------------------------------------------
-- 5) CONVENIENCE WRAPPERS (For GHCi testing)
--   run "2 * 5 + 9"        ==> Right 19
--   showPrefix "2 * 5 + 9" ==> Right "add (mult 2 5) 9"
-- -------------------------------------------------------------------------

run :: String -> Either String Integer
run = undefined  -- TODO: parseExprString then eval

showPrefix :: String -> Either String String
showPrefix = undefined  -- TODO: parseExprString then toPrefix

-- -------------------------------------------------------------------------
-- End of Starter
-- -------------------------------------------------------------------------
