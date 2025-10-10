import Data.Char
data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

data Expr
  = Lit Integer
  | Bin Op Expr Expr
  deriving (Eq, Show)

-- -------------------------------------------------------------------------
-- Tokens (unchanged)
-- -------------------------------------------------------------------------

data Token
  = TPlus | TMinus | TMul | TDiv
  | TLParen | TRParen
  | TInt Integer
  deriving (Eq, Show)

-- -------------------------------------------------------------------------
-- 1) TOKENIZER (unchanged)
-- -------------------------------------------------------------------------

tokenize :: String -> [Token]
tokenize expression = parseTokens "" expression

parseTokens :: String -> String -> [Token]
parseTokens tracking ""
    | null tracking = []
    | otherwise     = [TInt (read tracking)]

parseTokens tracking (cur:remaining)
    | cur == '*' = TMul : parseTokens "" remaining
    | cur == '/' = TDiv : parseTokens "" remaining
    | cur == '+' = TPlus : parseTokens "" remaining
    | cur == '-' = parseTokens (tracking ++ [cur]) remaining
    | cur == '(' && tracking == "-" = TInt (-1) : TMul : TLParen : parseTokens "" remaining
    | cur == '(' = TLParen : parseTokens "" remaining
    | cur == ')' && not (null tracking) = TInt (read tracking) : TRParen : parseTokens "" remaining
    | cur == ')' = TRParen : parseTokens "" remaining
    | isSpace cur && tracking == "-" = TMinus : parseTokens "" remaining
    | isSpace cur && not (null tracking) = TInt (read tracking) : parseTokens "" remaining
    | isSpace cur = parseTokens "" remaining
    | otherwise = parseTokens (tracking ++ [cur]) remaining

-- -------------------------------------------------------------------------
-- 2) PARSER (no error handling)
-- -------------------------------------------------------------------------

parseExprString :: String -> Expr
parseExprString s = 
    let tokens = tokenize s
        (expr, _) = parseAddSub tokens
    in expr

-- + / -
parseAddSub :: [Token] -> (Expr, [Token])
parseAddSub tokens = parseAddSubRest left rest
  where
    (left, rest) = parseMultDiv tokens

    parseAddSubRest :: Expr -> [Token] -> (Expr, [Token])
    parseAddSubRest leftExpr (TPlus:rest) = 
        let (rightExpr, rest2) = parseMultDiv rest
        in parseAddSubRest (Bin Add leftExpr rightExpr) rest2
    parseAddSubRest leftExpr (TMinus:rest) =
        let (rightExpr, rest2) = parseMultDiv rest
        in parseAddSubRest (Bin Sub leftExpr rightExpr) rest2
    parseAddSubRest leftExpr rest = (leftExpr, rest)

-- * / 
parseMultDiv :: [Token] -> (Expr, [Token])
parseMultDiv tokens = parseMultDivRest left rest
  where
    (left, rest) = parseConstant tokens

    parseMultDivRest :: Expr -> [Token] -> (Expr, [Token])
    parseMultDivRest leftExpr (TMul:rest) =
        let (rightExpr, rest2) = parseConstant rest
        in parseMultDivRest (Bin Mul leftExpr rightExpr) rest2
    parseMultDivRest leftExpr (TDiv:rest) =
        let (rightExpr, rest2) = parseConstant rest
        in parseMultDivRest (Bin Div leftExpr rightExpr) rest2
    parseMultDivRest leftExpr rest = (leftExpr, rest)

-- integer literal or parenthesized expr
parseConstant :: [Token] -> (Expr, [Token])
parseConstant (TInt n : rest) = (Lit n, rest)
parseConstant (TLParen : rest) = 
    let (expr, TRParen : rest2) = parseAddSub rest
    in (expr, rest2)
