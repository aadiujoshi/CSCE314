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
    let tokens = tokenize s 
    in case parseAddSub tokens of
        Left err -> Left err 
        Right (expr, []) -> Right expr
        Right (_, remaining) -> Left $ "Unexpected tokens at end: " ++ show remaining

parseAddSub :: [Token] -> Either String (Expr, [Token])
parseAddSub tokens = 
    case parseMultDiv tokens of
        Left err -> Left err  
        Right (leftExpr, rest) -> parseAddSubRest leftExpr rest
  where
    parseAddSubRest :: Expr -> [Token] -> Either String (Expr, [Token])
    parseAddSubRest leftExpr [] = Right (leftExpr, [])  
    
    parseAddSubRest leftExpr (TPlus:rest) = 
        case parseMultDiv rest of
            Left err -> Left err
            Right (rightExpr, rest2) -> 
                parseAddSubRest (Bin Add leftExpr rightExpr) rest2
    
    parseAddSubRest leftExpr (TMinus:rest) = 
        case parseMultDiv rest of
            Left err -> Left err
            Right (rightExpr, rest2) -> 
                parseAddSubRest (Bin Sub leftExpr rightExpr) rest2
    parseAddSubRest leftExpr rest = Right (leftExpr, rest)

parseMultDiv :: [Token] -> Either String (Expr, [Token])
parseMultDiv tokens = 
    case parseConstant tokens of
        Left err -> Left err
        Right (leftExpr, rest) -> parseMultDivRest leftExpr rest
  where
    parseMultDivRest :: Expr -> [Token] -> Either String (Expr, [Token])
    parseMultDivRest leftExpr [] = Right (leftExpr, [])
    
    parseMultDivRest leftExpr (TMul:rest) = 
        case parseConstant rest of
            Left err -> Left err
            Right (rightExpr, rest2) -> 
                parseMultDivRest (Bin Mul leftExpr rightExpr) rest2
    
    parseMultDivRest leftExpr (TDiv:rest) = 
        case parseConstant rest of
            Left err -> Left err
            Right (rightExpr, rest2) -> 
                parseMultDivRest (Bin Div leftExpr rightExpr) rest2
    parseMultDivRest leftExpr rest = Right (leftExpr, rest)

parseConstant :: [Token] -> Either String (Expr, [Token])
parseConstant [] = Left "Unexpected end of input in factor"

parseConstant (TInt n : rest) = Right (Lit n, rest)
parseConstant (TLParen : rest) = 
    case parseAddSub rest of 
        Left err -> Left err
        Right (expr, TRParen : rest2) -> 
            Right (expr, rest2)
        Right (_, rest2) -> 
            Left "Unclosed parenthesis"

parseConstant (tok : _) = Left $ "Unexpected token in factor: " ++ show tok

-- -------------------------------------------------------------------------
-- 3) PRETTY-PRINT (Prefix)
--   toPrefix (Lit 2) == "2"
--   toPrefix (Bin Add (Lit 2) (Lit 3)) == "add 2 3"
--   Wrap nested subexpressions in parentheses for readability:
--   toPrefix (Bin Add (Bin Mul (Lit 2) (Lit 5)) (Lit 9))
--     == "add (mult 2 5) 9"
-- -------------------------------------------------------------------------

toPrefix :: Expr -> String
toPrefix (Lit num) = show num
toPrefix (Bin operation left right) =
    opString ++ " " ++ leftStr ++ " " ++ rightStr
    where
        opString = case operation of
            Add -> "add"
            Sub -> "sub"
            Mul -> "mult"
            Div -> "div"

        leftStr  = wrapParen left
        rightStr = wrapParen right

        wrapParen (Lit n) = show n
        wrapParen expr    = "(" ++ toPrefix expr ++ ")"

-- -------------------------------------------------------------------------
-- 4) EVALUATION (Safe with Either)
--   eval (Lit 2) == Right 2
--   eval (Bin Div (Lit 8) (Lit 0)) == Left "Division by zero"
--   Use recursion and pattern matching
-- -------------------------------------------------------------------------

eval :: Expr -> Either String Integer
eval (Lit x) = Right x
eval (Bin operation left right) = 
    case (eval left, eval right) of
        (Left lerr, _) -> Left lerr
        (_, Left rerr) -> Left rerr
        (Right l2, Right r2) -> case operation of
            Div -> case r2 of
                0 -> Left "Division by zero"
                _ -> Right (l2 `div` r2)
            Mul -> Right (l2 * r2)
            Add -> Right (l2 + r2)
            Sub -> Right (l2 - r2)

-- -------------------------------------------------------------------------
-- 5) CONVENIENCE WRAPPERS (For GHCi testing)
--   run "2 * 5 + 9"        ==> Right 19
--   showPrefix "2 * 5 + 9" ==> Right "add (mult 2 5) 9"
-- -------------------------------------------------------------------------

run :: String -> Either String Integer
run s = do
    expr <- parseExprString s  
    eval expr                  

showPrefix :: String -> Either String String
showPrefix s = do
    expr <- parseExprString s
    return (toPrefix expr)

-- -------------------------------------------------------------------------
-- End of Starter
-- -------------------------------------------------------------------------
-- module HW_Expr_Tests where
-- import HW_Expr_solution

ok :: (Eq a, Show a) => String -> a -> a -> IO ()
ok name got want =
  putStrLn (name ++ ": " ++ if got == want then "OK" else "FAIL, got " ++ show got ++ ", want " ++ show want)

runTests :: IO ()
runTests = do
  ok "prefix 1"
     (showPrefix "2 * 5 + 9")
     (Right "add (mult 2 5) 9")

  ok "eval 1"
     (run "2 * 5 + 9")
     (Right 19)

  ok "prefix 2"
     (showPrefix "12 + (30 / 5) * 2")
     (Right "add 12 (mult (div 30 5) 2)")

  ok "eval 2"
     (run "12 + (30 / 5) * 2")
     (Right 24)

  ok "div by zero"
     (run "8 / (3 - 3)")
     (Left "Division by zero")
