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
-- Complete implementation with detailed comments for learning.
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
--    Converts a String into [Token], handling whitespace and multi-digit numbers.
-- -------------------------------------------------------------------------

tokenize :: String -> [Token]
tokenize expression = parseTokens "" expression

-- Helper function that maintains state while parsing
-- tracking: accumulates digits/minus for a number
-- remaining: characters left to process
-- returns: list of tokens parsed so far
parseTokens :: String -> String -> [Token]
-- Base case: no more characters to process
parseTokens tracking ""
    | null tracking = []  -- If tracking is empty, we're done
    | otherwise     = [TInt (read tracking)]  -- Convert accumulated string to integer

parseTokens tracking (cur:remaining)
    -- Multiplication operator: emit TMul and reset tracking
    | cur == '*' =                          TMul : parseTokens "" remaining
    
    -- Division operator: emit TDiv and reset tracking
    | cur == '/' =                          TDiv : parseTokens "" remaining
    
    -- Plus operator: emit TPlus and reset tracking
    | cur == '+' =                          TPlus : parseTokens "" remaining
    
    -- Minus: ambiguous! Could be subtraction or negative number
    -- Keep tracking it to decide later based on context
    | cur == '-' =                          parseTokens (tracking ++ [cur]) remaining
    
    -- Left paren after minus: treat minus as negative one multiplied
    -- This handles cases like -(expr) as -1 * (expr)
    | cur == '(' && tracking == "-" =       TInt (-1) : TMul : TLParen : parseTokens "" remaining
    
    -- Left paren without minus: just emit the paren
    | cur == '(' =                          TLParen : parseTokens "" remaining
    
    -- Right paren with accumulated number: emit the number then paren
    | cur == ')' && not (null tracking) =   TInt (read tracking) : TRParen : parseTokens "" remaining
    
    -- Right paren without number: just emit the paren
    | cur == ')' =                          TRParen : parseTokens "" remaining
    
    -- Space after lone minus: it's a subtraction operator
    | isSpace cur && tracking == "-" =      TMinus : parseTokens "" remaining
    
    -- Space after a number: convert accumulated string to integer
    | isSpace cur && not (null tracking) =  TInt (read tracking) : parseTokens "" remaining
    
    -- Space with nothing tracked: skip it
    | isSpace cur =                         parseTokens "" remaining
    
    -- Any other character (digit): accumulate it
    | otherwise =                           parseTokens (tracking ++ [cur]) remaining

-- -------------------------------------------------------------------------
-- 2) PARSER (Recursive Descent)
-- Grammar (order of operations encoded in nesting):
--   Expr   ::= Term (('+' | '-') Term)*     -- Lowest precedence
--   Term   ::= Factor (('*' | '/') Factor)* -- Higher precedence
--   Factor ::= INT | '(' Expr ')'           -- Highest precedence
--
-- Pattern: Each parse function returns Either String (Expr, [Token])
--   Left "error" for failures
--   Right (ast, remaining_tokens) for success
-- -------------------------------------------------------------------------

-- Main entry point: tokenize then parse
parseExprString :: String -> Either String Expr
parseExprString s = 
    let tokens = tokenize s  -- First, convert string to tokens
    in case parseAddSub tokens of
        Left err -> Left err  -- Propagate parse error
        Right (expr, []) -> Right expr  -- Success: no tokens left
        Right (_, remaining) -> Left $ "Unexpected tokens at end: " ++ show remaining

-- Parse an expression: handles + and - (lowest precedence)
parseAddSub :: [Token] -> Either String (Expr, [Token])
parseAddSub tokens = 
    -- First, parse a Term (which handles * and /)
    case parseMultDiv tokens of
        Left err -> Left err  -- If Term fails, propagate error
        Right (leftExpr, rest) -> parseAddSubRest leftExpr rest
  where
    -- parseExprRest: handles the repetition of ('+' | '-') Term
    -- leftExpr: the AST built so far (accumulator)
    -- tokens: remaining tokens to process
    parseAddSubRest :: Expr -> [Token] -> Either String (Expr, [Token])
    parseAddSubRest leftExpr [] = Right (leftExpr, [])  -- No more tokens: done
    
    parseAddSubRest leftExpr (TPlus:rest) = 
        -- Found +: parse the right-hand Term
        case parseMultDiv rest of
            Left err -> Left err
            -- Build a Bin Add node and continue parsing
            Right (rightExpr, rest2) -> 
                parseAddSubRest (Bin Add leftExpr rightExpr) rest2
    
    parseAddSubRest leftExpr (TMinus:rest) = 
        -- Found -: parse the right-hand Term
        case parseMultDiv rest of
            Left err -> Left err
            -- Build a Bin Sub node and continue parsing
            Right (rightExpr, rest2) -> 
                parseAddSubRest (Bin Sub leftExpr rightExpr) rest2
    
    -- Any other token: not part of this Expr level, return what we have
    parseAddSubRest leftExpr rest = Right (leftExpr, rest)

-- Parse a term: handles * and / (higher precedence than +/-)
parseMultDiv :: [Token] -> Either String (Expr, [Token])
parseMultDiv tokens = 
    -- First, parse a Factor (integer or parenthesized expression)
    case parseConstant tokens of
        Left err -> Left err
        Right (leftExpr, rest) -> parseMultDivRest leftExpr rest
  where
    -- parseTermRest: handles the repetition of ('*' | '/') Factor
    parseMultDivRest :: Expr -> [Token] -> Either String (Expr, [Token])
    parseMultDivRest leftExpr [] = Right (leftExpr, [])
    
    parseMultDivRest leftExpr (TMul:rest) = 
        -- Found *: parse the right-hand Factor q
        case parseConstant rest of
            Left err -> Left err
            -- Build a Bin Mul node and continue
            Right (rightExpr, rest2) -> 
                parseMultDivRest (Bin Mul leftExpr rightExpr) rest2
    
    parseMultDivRest leftExpr (TDiv:rest) = 
        -- Found /: parse the right-hand Factor
        case parseConstant rest of
            Left err -> Left err
            -- Build a Bin Div node and continue
            Right (rightExpr, rest2) -> 
                parseMultDivRest (Bin Div leftExpr rightExpr) rest2
    
    -- Any other token: not part of this Term level
    parseMultDivRest leftExpr rest = Right (leftExpr, rest)

-- Parse a factor: an integer literal or a parenthesized expression
parseConstant :: [Token] -> Either String (Expr, [Token])
parseConstant [] = Left "Unexpected end of input in factor"

-- Case 1: Integer literal
parseConstant (TInt n : rest) = Right (Lit n, rest)

-- Case 2: Parenthesized expression
parseConstant (TLParen : rest) = 
    case parseAddSub rest of  -- Recursively parse the inner expression
        Left err -> Left err
        Right (expr, TRParen : rest2) -> 
            -- Successfully found matching right paren
            Right (expr, rest2)
        Right (_, rest2) -> 
            -- Missing right paren
            Left $ "Expected ')' but found: " ++ show (take 1 rest2)

-- Case 3: Unexpected token
parseConstant (tok : _) = Left $ "Unexpected token in factor: " ++ show tok

-- -------------------------------------------------------------------------
-- 3) PRETTY-PRINT (Prefix notation)
--   Converts AST to prefix notation string (like LISP)
--   Examples:
--     2           => "2"
--     2 + 3       => "add 2 3"
--     (2 * 5) + 9 => "add (mult 2 5) 9"
-- -------------------------------------------------------------------------

toPrefix :: Expr -> String
-- Base case: literal integer
toPrefix (Lit n) = show n

-- Recursive case: binary operation
toPrefix (Bin op left right) = 
    -- Format: "operator left right"
    opName ++ " " ++ leftStr ++ " " ++ rightStr
  where
    -- Convert operator to string name
    opName = case op of
        Add -> "add"
        Sub -> "sub"
        Mul -> "mult"
        Div -> "div"
    
    -- Helper: wrap in parens if the subexpression is a binary operation
    -- This improves readability: "add (mult 2 5) 9" vs "add mult 2 5 9"
    wrapIfNeeded :: Expr -> String
    wrapIfNeeded e@(Bin _ _ _) = "(" ++ toPrefix e ++ ")"
    wrapIfNeeded e = toPrefix e
    
    leftStr = wrapIfNeeded left
    rightStr = wrapIfNeeded right

-- -------------------------------------------------------------------------
-- 4) EVALUATION (Safe with Either for error handling)
--   Recursively evaluates the AST to an integer value
--   Returns Left "error" for division by zero
-- -------------------------------------------------------------------------

eval :: Expr -> Either String Integer
-- Base case: literal integer evaluates to itself
eval (Lit n) = Right n

-- Recursive case: binary operation
eval (Bin op left right) = do
    -- The 'do' notation for Either allows us to chain computations
    -- If any step fails (returns Left), the whole computation fails
    
    -- Evaluate left subexpression
    leftVal <- eval left
    
    -- Evaluate right subexpression
    rightVal <- eval right
    
    -- Apply the operator, checking for division by zero
    case op of
        Add -> Right (leftVal + rightVal)
        Sub -> Right (leftVal - rightVal)
        Mul -> Right (leftVal * rightVal)
        Div -> 
            if rightVal == 0
            then Left "Division by zero"  -- Error case
            else Right (leftVal `div` rightVal)  -- Integer division

-- -------------------------------------------------------------------------
-- 5) CONVENIENCE WRAPPERS (For GHCi testing)
--   These compose the parsing and processing stages
-- -------------------------------------------------------------------------

-- Parse then evaluate: "2 * 5 + 9" => Right 19
run :: String -> Either String Integer
run s = do
    -- The 'do' notation chains Either computations
    expr <- parseExprString s  -- Parse string to AST
    eval expr                   -- Evaluate AST to integer

-- Parse then convert to prefix: "2 * 5 + 9" => Right "add (mult 2 5) 9"
showPrefix :: String -> Either String String
showPrefix s = do
    expr <- parseExprString s  -- Parse string to AST
    return (toPrefix expr)     -- Convert AST to prefix string
    -- Note: 'return' in Haskell wraps a value in a monad (Right here)

-- -------------------------------------------------------------------------
-- DETAILED PARSING TRACE EXAMPLE
-- -------------------------------------------------------------------------
{-
Example: "3 + 4 * 5"

STEP 1: TOKENIZATION
--------------------
Input string: "3 + 4 * 5"
After tokenize: [TInt 3, TPlus, TInt 4, TMul, TInt 5]

STEP 2: PARSING (Recursive Descent)
------------------------------------
Initial call: parseExprString [TInt 3, TPlus, TInt 4, TMul, TInt 5]
  |
  +--> parseExpr [TInt 3, TPlus, TInt 4, TMul, TInt 5]
        |
        | First, we must parse a Term (since Expr = Term (('+' | '-') Term)*)
        |
        +--> parseTerm [TInt 3, TPlus, TInt 4, TMul, TInt 5]
              |
              | First, we must parse a Factor (since Term = Factor (('*' | '/') Factor)*)
              |
              +--> parseFactor [TInt 3, TPlus, TInt 4, TMul, TInt 5]
                    |
                    | Matches pattern: (TInt n : rest)
                    | Returns: Right (Lit 3, [TPlus, TInt 4, TMul, TInt 5])
              |
              | Back in parseTerm with result: (Lit 3, [TPlus, TInt 4, TMul, TInt 5])
              | Now call parseTermRest (Lit 3) [TPlus, TInt 4, TMul, TInt 5]
              |
              +--> parseTermRest (Lit 3) [TPlus, TInt 4, TMul, TInt 5]
                    |
                    | Check first token: TPlus
                    | TPlus is NOT TMul or TDiv, so this Term is done
                    | Returns: Right (Lit 3, [TPlus, TInt 4, TMul, TInt 5])
        |
        | Back in parseExpr with result: (Lit 3, [TPlus, TInt 4, TMul, TInt 5])
        | Now call parseExprRest (Lit 3) [TPlus, TInt 4, TMul, TInt 5]
        |
        +--> parseExprRest (Lit 3) [TPlus, TInt 4, TMul, TInt 5]
              |
              | Matches pattern: (TPlus:rest)
              | Found a +, so parse the right-hand side Term
              |
              +--> parseTerm [TInt 4, TMul, TInt 5]
                    |
                    | First parse a Factor
                    |
                    +--> parseFactor [TInt 4, TMul, TInt 5]
                          |
                          | Matches: (TInt 4 : rest)
                          | Returns: Right (Lit 4, [TMul, TInt 5])
                    |
                    | Back in parseTerm with: (Lit 4, [TMul, TInt 5])
                    | Call parseTermRest (Lit 4) [TMul, TInt 5]
                    |
                    +--> parseTermRest (Lit 4) [TMul, TInt 5]
                          |
                          | Matches pattern: (TMul:rest)
                          | Found a *, so parse the right-hand Factor
                          |
                          +--> parseFactor [TInt 5]
                                |
                                | Matches: (TInt 5 : rest)
                                | Returns: Right (Lit 5, [])
                          |
                          | Back in parseTermRest with: (Lit 5, [])
                          | Build: Bin Mul (Lit 4) (Lit 5)
                          | Continue: parseTermRest (Bin Mul (Lit 4) (Lit 5)) []
                          |
                          +--> parseTermRest (Bin Mul (Lit 4) (Lit 5)) []
                                |
                                | Token list is empty
                                | Returns: Right (Bin Mul (Lit 4) (Lit 5), [])
                    |
                    | parseTerm returns: Right (Bin Mul (Lit 4) (Lit 5), [])
              |
              | Back in parseExprRest with right side: Bin Mul (Lit 4) (Lit 5)
              | Build: Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5))
              | Continue: parseExprRest (Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5))) []
              |
              +--> parseExprRest (Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5))) []
                    |
                    | Token list is empty
                    | Returns: Right (Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5)), [])
        |
        | parseExpr returns: Right (Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5)), [])
  |
  | parseExprString checks: no remaining tokens
  | Final result: Right (Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5)))

STEP 3: FINAL AST VISUALIZATION
--------------------------------
The resulting AST has the structure:

        Add
       /   \
     Lit(3) Mul
           /   \
        Lit(4) Lit(5)

This correctly represents: 3 + (4 * 5)
The multiplication has higher precedence because it's parsed deeper in the tree.

STEP 4: EVALUATION TRACE
-------------------------
eval (Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5)))
  |
  +--> Evaluate left: eval (Lit 3) = Right 3
  |
  +--> Evaluate right: eval (Bin Mul (Lit 4) (Lit 5))
        |
        +--> Evaluate left: eval (Lit 4) = Right 4
        |
        +--> Evaluate right: eval (Lit 5) = Right 5
        |
        +--> Apply Mul: 4 * 5 = Right 20
  |
  +--> Apply Add: 3 + 20 = Right 23

Final result: Right 23

STEP 5: PREFIX NOTATION TRACE
------------------------------
toPrefix (Bin Add (Lit 3) (Bin Mul (Lit 4) (Lit 5)))
  |
  +--> opName = "add"
  |
  +--> leftStr = toPrefix (Lit 3) = "3"
  |
  +--> rightStr = wrapIfNeeded (Bin Mul (Lit 4) (Lit 5))
        |
        | This is a Bin, so wrap in parens
        |
        +--> "(" ++ toPrefix (Bin Mul (Lit 4) (Lit 5)) ++ ")"
              |
              +--> opName = "mult"
              |
              +--> leftStr = toPrefix (Lit 4) = "4"
              |
              +--> rightStr = toPrefix (Lit 5) = "5"
              |
              +--> Result: "mult 4 5"
        |
        +--> Wrapped result: "(mult 4 5)"
  |
  +--> Final: "add 3 (mult 4 5)"

KEY INSIGHTS:
-------------
1. Parser respects operator precedence through grammar structure:
   - Expr handles + and - (lowest precedence)
   - Term handles * and / (higher precedence)
   - Factor handles atoms and parentheses (highest precedence)

2. Left-associativity is handled by the "Rest" functions:
   - They build the tree left-to-right using an accumulator
   - Example: "1 + 2 + 3" becomes Add(Add(1, 2), 3) not Add(1, Add(2, 3))

3. The recursive descent naturally creates the AST:
   - Lower precedence operations end up higher in the tree
   - Higher precedence operations end up deeper in the tree
   - This ensures correct evaluation order

4. Error propagation with Either:
   - Any Left value short-circuits the entire computation
   - Success values are threaded through using monadic bind (<-)
-}

-- -------------------------------------------------------------------------
-- End of Implementation
-- -------------------------------------------------------------------------