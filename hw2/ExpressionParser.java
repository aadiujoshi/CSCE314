import java.util.*;

/**
 * Complete Arithmetic Expression Parser - Java Implementation
 * Converts string expressions like "3 + 4 * 5" into AST and evaluates them.
 * 
 * This is a full conversion from the Haskell implementation, demonstrating
 * how functional programming patterns translate to object-oriented Java.
 */

// ============================================================================
// OPERATORS ENUM
// ============================================================================

/**
 * Represents binary operators: +, -, *, /
 * In Haskell this was: data Op = Add | Sub | Mul | Div
 */
enum Op {
    ADD, SUB, MUL, DIV;
    
    @Override
    public String toString() {
        switch (this) {
            case ADD: return "add";
            case SUB: return "sub";
            case MUL: return "mult";
            case DIV: return "div";
            default: return "unknown";
        }
    }
}

// ============================================================================
// EXPRESSION AST (Abstract Syntax Tree)
// ============================================================================

/**
 * Base class for expressions.
 * In Haskell: data Expr = Lit Integer | Bin Op Expr Expr
 * 
 * Java uses inheritance instead of algebraic data types.
 */
abstract class Expr {
    /**
     * Convert this expression to prefix notation.
     * Example: (2 + 3) becomes "add 2 3"
     */
    public abstract String toPrefix();
    
    /**
     * Evaluate this expression to an integer.
     * Returns a Result type that can hold either a value or an error.
     */
    public abstract Result<Integer> eval();
}

/**
 * Literal integer expression.
 * In Haskell: Lit Integer
 */
class Lit extends Expr {
    private final int value;
    
    public Lit(int value) {
        this.value = value;
    }
    
    public int getValue() {
        return value;
    }
    
    @Override
    public String toPrefix() {
        return String.valueOf(value);
    }
    
    @Override
    public Result<Integer> eval() {
        // A literal always evaluates to itself successfully
        return Result.ok(value);
    }
    
    @Override
    public String toString() {
        return "Lit(" + value + ")";
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Lit)) return false;
        Lit lit = (Lit) o;
        return value == lit.value;
    }
}

/**
 * Binary operation expression.
 * In Haskell: Bin Op Expr Expr
 */
class Bin extends Expr {
    private final Op op;
    private final Expr left;
    private final Expr right;
    
    public Bin(Op op, Expr left, Expr right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }
    
    public Op getOp() { return op; }
    public Expr getLeft() { return left; }
    public Expr getRight() { return right; }
    
    @Override
    public String toPrefix() {
        // Format: "operator left right"
        // Wrap nested binary operations in parentheses for readability
        String leftStr = (left instanceof Bin) 
            ? "(" + left.toPrefix() + ")" 
            : left.toPrefix();
        
        String rightStr = (right instanceof Bin) 
            ? "(" + right.toPrefix() + ")" 
            : right.toPrefix();
        
        return op.toString() + " " + leftStr + " " + rightStr;
    }
    
    @Override
    public Result<Integer> eval() {
        // Evaluate left subexpression
        Result<Integer> leftResult = left.eval();
        if (leftResult.isError()) {
            return leftResult; // Propagate error
        }
        
        // Evaluate right subexpression
        Result<Integer> rightResult = right.eval();
        if (rightResult.isError()) {
            return rightResult; // Propagate error
        }
        
        // Both succeeded, extract values
        int leftVal = leftResult.getValue();
        int rightVal = rightResult.getValue();
        
        // Apply the operation
        switch (op) {
            case ADD:
                return Result.ok(leftVal + rightVal);
            case SUB:
                return Result.ok(leftVal - rightVal);
            case MUL:
                return Result.ok(leftVal * rightVal);
            case DIV:
                if (rightVal == 0) {
                    return Result.error("Division by zero");
                }
                return Result.ok(leftVal / rightVal);
            default:
                return Result.error("Unknown operator");
        }
    }
    
    @Override
    public String toString() {
        return "Bin(" + op + ", " + left + ", " + right + ")";
    }
}

// ============================================================================
// TOKENS
// ============================================================================

/**
 * Represents individual tokens from the input string.
 * In Haskell: data Token = TPlus | TMinus | ... | TInt Integer
 */
abstract class Token {
    @Override
    public abstract String toString();
}

class TPlus extends Token {
    @Override public String toString() { return "TPlus"; }
}

class TMinus extends Token {
    @Override public String toString() { return "TMinus"; }
}

class TMul extends Token {
    @Override public String toString() { return "TMul"; }
}

class TDiv extends Token {
    @Override public String toString() { return "TDiv"; }
}

class TLParen extends Token {
    @Override public String toString() { return "TLParen"; }
}

class TRParen extends Token {
    @Override public String toString() { return "TRParen"; }
}

class TInt extends Token {
    private final int value;
    
    public TInt(int value) {
        this.value = value;
    }
    
    public int getValue() {
        return value;
    }
    
    @Override
    public String toString() {
        return "TInt(" + value + ")";
    }
}

// ============================================================================
// RESULT TYPE (Simulates Haskell's Either for error handling)
// ============================================================================

/**
 * Generic result type that can hold either a success value or an error message.
 * In Haskell: Either String a
 * 
 * This is Java's way of handling operations that might fail without exceptions.
 */
class Result<T> {
    private final T value;
    private final String error;
    private final boolean isOk;
    
    private Result(T value, String error, boolean isOk) {
        this.value = value;
        this.error = error;
        this.isOk = isOk;
    }
    
    /**
     * Create a successful result.
     * In Haskell: Right value
     */
    public static <T> Result<T> ok(T value) {
        return new Result<>(value, null, true);
    }
    
    /**
     * Create an error result.
     * In Haskell: Left "error message"
     */
    public static <T> Result<T> error(String message) {
        return new Result<>(null, message, false);
    }
    
    public boolean isOk() {
        return isOk;
    }
    
    public boolean isError() {
        return !isOk;
    }
    
    public T getValue() {
        if (!isOk) {
            throw new IllegalStateException("Cannot get value from error result");
        }
        return value;
    }
    
    public String getError() {
        if (isOk) {
            throw new IllegalStateException("Cannot get error from ok result");
        }
        return error;
    }
    
    /**
     * Map function for transforming success values.
     * In Haskell: fmap
     */
    public <U> Result<U> map(java.util.function.Function<T, U> f) {
        if (isOk) {
            return Result.ok(f.apply(value));
        } else {
            return Result.error(error);
        }
    }
    
    @Override
    public String toString() {
        if (isOk) {
            return "Ok(" + value + ")";
        } else {
            return "Error(" + error + ")";
        }
    }
}

// ============================================================================
// TOKENIZER
// ============================================================================

/**
 * Converts a string like "3 + 4 * 5" into a list of tokens.
 * Handles multi-digit numbers, negative numbers, and whitespace.
 */
class Tokenizer {
    
    /**
     * Main tokenization function.
     * Example: "3 + 4" -> [TInt(3), TPlus, TInt(4)]
     */
    public static List<Token> tokenize(String input) {
        return parseTokens("", input, 0);
    }
    
    /**
     * Recursive helper that processes the string character by character.
     * 
     * @param tracking Accumulates digits/minus for building numbers
     * @param input The original input string
     * @param pos Current position in the string
     * @return List of tokens parsed so far
     */
    private static List<Token> parseTokens(String tracking, String input, int pos) {
        // Base case: reached end of string
        if (pos >= input.length()) {
            if (tracking.isEmpty()) {
                return new ArrayList<>();
            } else {
                // Convert accumulated string to integer token
                List<Token> result = new ArrayList<>();
                result.add(new TInt(Integer.parseInt(tracking)));
                return result;
            }
        }
        
        char cur = input.charAt(pos);
        List<Token> result = new ArrayList<>();
        
        // Multiplication operator
        if (cur == '*') {
            result.add(new TMul());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Division operator
        if (cur == '/') {
            result.add(new TDiv());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Plus operator
        if (cur == '+') {
            result.add(new TPlus());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Minus: could be subtraction or negative number, keep tracking
        if (cur == '-') {
            return parseTokens(tracking + cur, input, pos + 1);
        }
        
        // Left paren after minus: treat as -1 * (expr)
        if (cur == '(' && tracking.equals("-")) {
            result.add(new TInt(-1));
            result.add(new TMul());
            result.add(new TLParen());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Left paren (normal case)
        if (cur == '(') {
            result.add(new TLParen());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Right paren with accumulated number
        if (cur == ')' && !tracking.isEmpty()) {
            result.add(new TInt(Integer.parseInt(tracking)));
            result.add(new TRParen());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Right paren (normal case)
        if (cur == ')') {
            result.add(new TRParen());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Space after lone minus: it's subtraction
        if (Character.isWhitespace(cur) && tracking.equals("-")) {
            result.add(new TMinus());
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Space after a number
        if (Character.isWhitespace(cur) && !tracking.isEmpty()) {
            result.add(new TInt(Integer.parseInt(tracking)));
            result.addAll(parseTokens("", input, pos + 1));
            return result;
        }
        
        // Space with nothing tracked
        if (Character.isWhitespace(cur)) {
            return parseTokens("", input, pos + 1);
        }
        
        // Any other character (digit): accumulate it
        return parseTokens(tracking + cur, input, pos + 1);
    }
}

// ============================================================================
// PARSER (Recursive Descent)
// ============================================================================

/**
 * Parses a list of tokens into an Abstract Syntax Tree (AST).
 * 
 * Grammar (operator precedence encoded in structure):
 *   Expr   ::= Term (('+' | '-') Term)*     // Lowest precedence
 *   Term   ::= Factor (('*' | '/') Factor)* // Higher precedence
 *   Factor ::= INT | '(' Expr ')'           // Highest precedence
 */
class Parser {
    
    /**
     * Helper class to return both an expression and remaining tokens.
     * In Haskell: (Expr, [Token])
     */
    static class ParseResult {
        final Expr expr;
        final List<Token> remaining;
        
        ParseResult(Expr expr, List<Token> remaining) {
            this.expr = expr;
            this.remaining = remaining;
        }
    }
    
    /**
     * Main entry point: parse a string into an expression.
     * Example: "3 + 4 * 5" -> Bin(Add, Lit(3), Bin(Mul, Lit(4), Lit(5)))
     */
    public static Result<Expr> parseExprString(String input) {
        // First, tokenize the input
        List<Token> tokens = Tokenizer.tokenize(input);
        
        // Then, parse the tokens
        Result<ParseResult> result = parseExpr(tokens);
        
        if (result.isError()) {
            return Result.error(result.getError());
        }
        
        ParseResult pr = result.getValue();
        
        // Check that we consumed all tokens
        if (!pr.remaining.isEmpty()) {
            return Result.error("Unexpected tokens at end: " + pr.remaining);
        }
        
        return Result.ok(pr.expr);
    }
    
    /**
     * Parse an expression: handles + and - (lowest precedence).
     * In Haskell: parseExpr :: [Token] -> Either String (Expr, [Token])
     */
    public static Result<ParseResult> parseExpr(List<Token> tokens) {
        // First, parse a Term
        Result<ParseResult> termResult = parseTerm(tokens);
        if (termResult.isError()) {
            return termResult;
        }
        
        ParseResult pr = termResult.getValue();
        
        // Then handle the repetition: (('+' | '-') Term)*
        return parseExprRest(pr.expr, pr.remaining);
    }
    
    /**
     * Helper for parseExpr: handles the repetition of ('+' | '-') Term.
     * Uses an accumulator pattern for left-associativity.
     * 
     * @param leftExpr The expression built so far (accumulator)
     * @param tokens Remaining tokens to process
     */
    private static Result<ParseResult> parseExprRest(Expr leftExpr, List<Token> tokens) {
        // Base case: no more tokens
        if (tokens.isEmpty()) {
            return Result.ok(new ParseResult(leftExpr, tokens));
        }
        
        Token first = tokens.get(0);
        List<Token> rest = tokens.subList(1, tokens.size());
        
        // Found '+': parse right-hand Term and build Add node
        if (first instanceof TPlus) {
            Result<ParseResult> termResult = parseTerm(rest);
            if (termResult.isError()) {
                return termResult;
            }
            
            ParseResult pr = termResult.getValue();
            Expr newExpr = new Bin(Op.ADD, leftExpr, pr.expr);
            
            // Continue parsing (left-associative)
            return parseExprRest(newExpr, pr.remaining);
        }
        
        // Found '-': parse right-hand Term and build Sub node
        if (first instanceof TMinus) {
            Result<ParseResult> termResult = parseTerm(rest);
            if (termResult.isError()) {
                return termResult;
            }
            
            ParseResult pr = termResult.getValue();
            Expr newExpr = new Bin(Op.SUB, leftExpr, pr.expr);
            
            // Continue parsing (left-associative)
            return parseExprRest(newExpr, pr.remaining);
        }
        
        // Not a + or -, so this Expr level is done
        return Result.ok(new ParseResult(leftExpr, tokens));
    }
    
    /**
     * Parse a term: handles * and / (higher precedence than +/-).
     */
    public static Result<ParseResult> parseTerm(List<Token> tokens) {
        // First, parse a Factor
        Result<ParseResult> factorResult = parseFactor(tokens);
        if (factorResult.isError()) {
            return factorResult;
        }
        
        ParseResult pr = factorResult.getValue();
        
        // Then handle the repetition: (('*' | '/') Factor)*
        return parseTermRest(pr.expr, pr.remaining);
    }
    
    /**
     * Helper for parseTerm: handles the repetition of ('*' | '/') Factor.
     */
    private static Result<ParseResult> parseTermRest(Expr leftExpr, List<Token> tokens) {
        // Base case: no more tokens
        if (tokens.isEmpty()) {
            return Result.ok(new ParseResult(leftExpr, tokens));
        }
        
        Token first = tokens.get(0);
        List<Token> rest = tokens.subList(1, tokens.size());
        
        // Found '*': parse right-hand Factor and build Mul node
        if (first instanceof TMul) {
            Result<ParseResult> factorResult = parseFactor(rest);
            if (factorResult.isError()) {
                return factorResult;
            }
            
            ParseResult pr = factorResult.getValue();
            Expr newExpr = new Bin(Op.MUL, leftExpr, pr.expr);
            
            // Continue parsing (left-associative)
            return parseTermRest(newExpr, pr.remaining);
        }
        
        // Found '/': parse right-hand Factor and build Div node
        if (first instanceof TDiv) {
            Result<ParseResult> factorResult = parseFactor(rest);
            if (factorResult.isError()) {
                return factorResult;
            }
            
            ParseResult pr = factorResult.getValue();
            Expr newExpr = new Bin(Op.DIV, leftExpr, pr.expr);
            
            // Continue parsing (left-associative)
            return parseTermRest(newExpr, pr.remaining);
        }
        
        // Not a * or /, so this Term level is done
        return Result.ok(new ParseResult(leftExpr, tokens));
    }
    
    /**
     * Parse a factor: an integer literal or parenthesized expression.
     */
    public static Result<ParseResult> parseFactor(List<Token> tokens) {
        // Error: no tokens left
        if (tokens.isEmpty()) {
            return Result.error("Unexpected end of input in factor");
        }
        
        Token first = tokens.get(0);
        List<Token> rest = tokens.subList(1, tokens.size());
        
        // Case 1: Integer literal
        if (first instanceof TInt) {
            int value = ((TInt) first).getValue();
            return Result.ok(new ParseResult(new Lit(value), rest));
        }
        
        // Case 2: Parenthesized expression
        if (first instanceof TLParen) {
            // Parse the inner expression
            Result<ParseResult> exprResult = parseExpr(rest);
            if (exprResult.isError()) {
                return exprResult;
            }
            
            ParseResult pr = exprResult.getValue();
            
            // Check for matching right paren
            if (pr.remaining.isEmpty()) {
                return Result.error("Expected ')' but reached end of input");
            }
            
            Token next = pr.remaining.get(0);
            if (!(next instanceof TRParen)) {
                return Result.error("Expected ')' but found: " + next);
            }
            
            List<Token> afterParen = pr.remaining.subList(1, pr.remaining.size());
            return Result.ok(new ParseResult(pr.expr, afterParen));
        }
        
        // Case 3: Unexpected token
        return Result.error("Unexpected token in factor: " + first);
    }
}

// ============================================================================
// CONVENIENCE FUNCTIONS
// ============================================================================

class ExprUtils {
    
    /**
     * Parse and evaluate an expression string.
     * Example: "3 + 4 * 5" -> Ok(23)
     */
    public static Result<Integer> run(String input) {
        Result<Expr> parseResult = Parser.parseExprString(input);
        
        if (parseResult.isError()) {
            return Result.error(parseResult.getError());
        }
        
        return parseResult.getValue().eval();
    }
    
    /**
     * Parse and convert to prefix notation.
     * Example: "3 + 4 * 5" -> Ok("add 3 (mult 4 5)")
     */
    public static Result<String> showPrefix(String input) {
        Result<Expr> parseResult = Parser.parseExprString(input);
        
        if (parseResult.isError()) {
            return Result.error(parseResult.getError());
        }
        
        return Result.ok(parseResult.getValue().toPrefix());
    }
}

// ============================================================================
// MAIN CLASS WITH EXAMPLES
// ============================================================================

public class ExpressionParser {
    
    public static void main(String[] args) {
        System.out.println("=== Arithmetic Expression Parser ===\n");
        
        // Test cases
        String[] testCases = {
            "3 + 4 * 5",
            "2 * 5 + 9",
            "(2 + 3) * 4",
            "10 / 2 - 3",
            "8 / 0",
            "-5 + 3",
            "-(10 * 3)",
            "1 + 2 + 3 + 4"
        };
        
        for (String expr : testCases) {
            System.out.println("Expression: " + expr);
            
            // Tokenize
            List<Token> tokens = Tokenizer.tokenize(expr);
            System.out.println("Tokens:     " + tokens);
            
            // Parse to AST
            Result<Expr> parseResult = Parser.parseExprString(expr);
            if (parseResult.isOk()) {
                Expr ast = parseResult.getValue();
                System.out.println("AST:        " + ast);
                
                // Convert to prefix
                System.out.println("Prefix:     " + ast.toPrefix());
                
                // Evaluate
                Result<Integer> evalResult = ast.eval();
                System.out.println("Result:     " + evalResult);
            } else {
                System.out.println("Parse Error: " + parseResult.getError());
            }
            
            System.out.println();
        }
        
        // Demonstrate the convenience functions
        System.out.println("=== Using Convenience Functions ===\n");
        
        System.out.println("run(\"3 + 4 * 5\") = " + ExprUtils.run("3 + 4 * 5"));
        System.out.println("showPrefix(\"3 + 4 * 5\") = " + ExprUtils.showPrefix("3 + 4 * 5"));
    }
}