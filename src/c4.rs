// src/c4.rs
use std::collections::HashMap;

/// Represents the possible tokens produced by the lexer.
///
/// Tokens are the basic building blocks of the source code, such as numbers,
/// identifiers, keywords, operators, strings, preprocessor directives, and an
/// end-of-file marker. This enum ensures type safety and clarity in token handling.
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(i64),
    Id(String),
    Keyword(String),
    Op(String),
    String(String),
    Directive(String),
    Eof,
}

/// A lexer that tokenizes C source code into a stream of `Token`s.
///
/// The lexer processes the input character by character, recognizing C language
/// constructs like keywords, identifiers, and operators. It maintains state such
/// as the current position and line number for error reporting.
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: i32,
    keywords: HashMap<String, Token>,
}

impl Lexer {
    /// Creates a new `Lexer` instance from a source string.
    ///
    /// Initializes the lexer with the input string and a predefined set of C
    /// keywords. The input is converted to a `Vec<char>` for efficient character
    /// access, and keywords are stored in a `HashMap` for O(1) lookup.
    ///
    /// # Arguments
    /// * `input` - The C source code to tokenize.
    ///
    /// # Returns
    /// A new `Lexer` instance ready to tokenize the input.
    pub fn new(input: &str) -> Self {
        let mut keywords = HashMap::new();
        for kw in ["int", "if", "while", "return", "char", "else"].iter() {
            keywords.insert(kw.to_string(), Token::Keyword(kw.to_string()));
        }
        Lexer {
            input: input.chars().collect(),
            pos: 0,
            line: 1,
            keywords,
        }
    }

    /// Returns the current line number in the source code.
    ///
    /// Useful for error reporting to indicate where in the source an issue occurred.
    ///
    /// # Returns
    /// The current line number as an `i32`.
    pub fn line(&self) -> i32 {
        self.line
    }

    /// Retrieves the next token from the input source.
    ///
    /// Skips whitespace and comments, then identifies and returns the next token.
    /// Supports C features like hexadecimal and octal numbers, string literals with
    /// escapes, and multi-character operators (e.g., `==`, `&&`). Errors are returned
    /// as `Result::Err` with descriptive messages, leveraging Rust's error handling.
    ///
    /// # Returns
    /// * `Ok(Token)` - The next token in the input.
    /// * `Err(String)` - An error message if tokenization fails (e.g., unexpected character).
    pub fn next(&mut self) -> Result<Token, String> {
        while self.pos < self.input.len() && self.input[self.pos].is_whitespace() {
            if self.input[self.pos] == '\n' {
                self.line += 1;
            }
            self.pos += 1;
        }

        if self.pos >= self.input.len() {
            return Ok(Token::Eof);
        }

        let c = self.input[self.pos];
        self.pos += 1;

        if c == '/' && self.pos < self.input.len() && self.input[self.pos] == '/' {
            self.pos += 1;
            while self.pos < self.input.len() && self.input[self.pos] != '\n' {
                self.pos += 1;
            }
            if self.pos < self.input.len() && self.input[self.pos] == '\n' {
                self.line += 1;
                self.pos += 1;
            }
            return self.next();
        }

        if c == '#' {
            let start = self.pos;
            while self.pos < self.input.len() && self.input[self.pos].is_alphabetic() {
                self.pos += 1;
            }
            let directive = self.input[start..self.pos].iter().collect::<String>();
            while self.pos < self.input.len() && self.input[self.pos] != '\n' {
                self.pos += 1;
            }
            if self.pos < self.input.len() && self.input[self.pos] == '\n' {
                self.line += 1;
                self.pos += 1;
            }
            if directive.is_empty() {
                return Err(format!("Empty preprocessor directive at line {}", self.line - 1));
            }
            return Ok(Token::Directive(directive));
        }

        if c.is_alphabetic() || c == '_' {
            let start = self.pos - 1;
            while self.pos < self.input.len() && (self.input[self.pos].is_alphanumeric() || self.input[self.pos] == '_') {
                self.pos += 1;
            }
            let id = self.input[start..self.pos].iter().collect::<String>();
            return Ok(self.keywords.get(&id).cloned().unwrap_or(Token::Id(id)));
        }

        if c.is_digit(10) {
            let start = self.pos - 1;
            if c == '0' && self.pos < self.input.len() && self.input[self.pos].to_ascii_lowercase() == 'x' {
                self.pos += 1;
                let hex_start = self.pos;
                while self.pos < self.input.len() && self.input[self.pos].is_digit(16) {
                    self.pos += 1;
                }
                let hex_str = self.input[hex_start..self.pos].iter().collect::<String>();
                return Ok(Token::Num(i64::from_str_radix(&hex_str, 16).unwrap_or(0)));
            } else if c == '0' && self.pos < self.input.len() && self.input[self.pos].is_digit(8) {
                let octal_start = self.pos;
                while self.pos < self.input.len() && self.input[self.pos].is_digit(8) {
                    self.pos += 1;
                }
                let octal_str = self.input[octal_start..self.pos].iter().collect::<String>();
                return Ok(Token::Num(i64::from_str_radix(&octal_str, 8).unwrap_or(0)));
            } else {
                while self.pos < self.input.len() && self.input[self.pos].is_digit(10) {
                    self.pos += 1;
                }
                let num_str = self.input[start..self.pos].iter().collect::<String>();
                return Ok(Token::Num(num_str.parse().unwrap_or(0)));
            }
        }

        if "+-*/=<>!&|~(){};,".contains(c) {
            let mut op = c.to_string();
            if self.pos < self.input.len() {
                match (c, self.input[self.pos]) {
                    ('=', '=') | ('!', '=') | ('<', '=') | ('>', '=') | ('&', '&') | ('|', '|') => {
                        op.push(self.input[self.pos]);
                        self.pos += 1;
                    }
                    _ => {}
                }
            }
            return Ok(Token::Op(op));
        }

        if c == '"' {
            let mut s = String::new();
            while self.pos < self.input.len() && self.input[self.pos] != '"' {
                if self.input[self.pos] == '\\' {
                    self.pos += 1;
                    if self.pos >= self.input.len() {
                        return Err(format!("Incomplete escape sequence at line {}", self.line));
                    }
                    match self.input[self.pos] {
                        'n' => s.push('\n'),
                        '"' => s.push('"'),
                        '\\' => s.push('\\'),
                        _ => {
                            s.push('\\');
                            s.push(self.input[self.pos]);
                        }
                    }
                    self.pos += 1;
                } else {
                    s.push(self.input[self.pos]);
                    self.pos += 1;
                }
            }
            if self.pos >= self.input.len() {
                return Err(format!("Unclosed string literal at line {}", self.line));
            }
            self.pos += 1;
            return Ok(Token::String(s));
        }

        Err(format!("Unexpected character '{}' at line {}", c, self.line))
    }
}

/// Represents an expression in the C abstract syntax tree (AST).
///
/// Expressions include literals, variables, function calls, and operations.
/// Using `Box` for recursive variants ensures memory safety and prevents stack
/// overflow, aligning with Rust's ownership model.
#[derive(Debug, PartialEq)]
pub enum Expr {
    Num(i64),
    String(String),
    Var(String),
    Call(String, Vec<Expr>),
    UnaryOp(String, Box<Expr>),
    BinOp(Box<Expr>, String, Box<Expr>),
}

/// Represents a statement in the C abstract syntax tree (AST).
///
/// Statements include expressions, assignments, returns, and control flow
/// constructs like `if` and `while`. This structure mirrors the subset of C
/// supported by the original C4 compiler.
#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Assign(String, Expr),
    Return(Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    While(Expr, Vec<Stmt>),
}

/// A parser that constructs an AST from a stream of tokens.
///
/// Uses a recursive descent approach to parse the C subset supported by C4.
/// Maintains compatibility by handling the same syntax and semantics as the
/// original C implementation.
pub struct Parser {
    lexer: Lexer,
    current_token: Result<Token, String>,
}

impl Parser {
    /// Creates a new `Parser` instance from a source string.
    ///
    /// Initializes the parser with a `Lexer` and fetches the first token.
    ///
    /// # Arguments
    /// * `input` - The C source code to parse.
    ///
    /// # Returns
    /// A new `Parser` instance ready to parse the input.
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next();
        Parser {
            lexer,
            current_token,
        }
    }

    /// Advances the parser to the next token.
    ///
    /// Updates `current_token` with the next token from the lexer.
    ///
    /// # Returns
    /// * `Ok(())` - If advancing succeeds.
    /// * `Err(String)` - If the lexer encounters an error.
    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.lexer.next();
        Ok(())
    }

    /// Parses a block of statements enclosed in braces or a single statement.
    ///
    /// Handles both braced blocks (e.g., `{ stmt; stmt; }`) and unbraced single
    /// statements, ensuring flexibility and compatibility with C4's syntax.
    ///
    /// # Returns
    /// * `Ok(Vec<Stmt>)` - A vector of parsed statements.
    /// * `Err(String)` - An error message if parsing fails (e.g., unmatched braces).
    pub fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();
        let mut expect_closing_brace = false;
    
        if let Ok(Token::Op(op)) = &self.current_token {
            if op == "{" {
                expect_closing_brace = true;
                self.advance()?;
            }
        }
    
        while let Ok(token) = &self.current_token {
            match token {
                Token::Op(op) if op == "}" => {
                    if !expect_closing_brace {
                        return Err(format!("Unexpected '}}' at line {}", self.lexer.line()));
                    }
                    self.advance()?;
                    break;
                }
                Token::Eof => {
                    if expect_closing_brace {
                        return Err(format!("Expected '}}' at end of block at line {}", self.lexer.line()));
                    }
                    break;
                }
                _ => stmts.push(self.parse_stmt()?),
            }
        }
    
        Ok(stmts)
    }

    /// Parses a single statement.
    ///
    /// Recognizes statements like `if`, `while`, `return`, assignments, and
    /// expressions, ensuring the parser supports C4's control flow and operations.
    ///
    /// # Returns
    /// * `Ok(Stmt)` - The parsed statement.
    /// * `Err(String)` - An error message if the statement is malformed.
    pub fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match self.current_token.clone() {
            Ok(Token::Keyword(kw)) if kw == "if" => {
                self.advance()?;
                if let Ok(Token::Op(op)) = &self.current_token {
                    if op != "(" {
                        return Err(format!("Expected '(' after 'if' at line {}", self.lexer.line()));
                    }
                } else {
                    return Err(format!("Expected '(' after 'if' at line {}", self.lexer.line()));
                }
                self.advance()?;
                let cond = self.parse_expr()?;
                if let Ok(Token::Op(op)) = &self.current_token {
                    if op != ")" {
                        return Err(format!("Expected ')' after if condition at line {}", self.lexer.line()));
                    }
                } else {
                    return Err(format!("Expected ')' after if condition at line {}", self.lexer.line()));
                }
                self.advance()?;
                let then_block = self.parse_block()?;
                let else_block = if let Ok(Token::Keyword(kw)) = &self.current_token {
                    if kw == "else" {
                        self.advance()?;
                        Some(self.parse_block()?)
                    } else {
                        None
                    }
                } else {
                    None
                };
                Ok(Stmt::If(cond, then_block, else_block))
            }
            Ok(Token::Keyword(kw)) if kw == "while" => {
                self.advance()?;
                if let Ok(Token::Op(op)) = &self.current_token {
                    if op != "(" {
                        return Err(format!("Expected '(' after 'while' at line {}", self.lexer.line()));
                    }
                } else {
                    return Err(format!("Expected '(' after 'while' at line {}", self.lexer.line()));
                }
                self.advance()?;
                let cond = self.parse_expr()?;
                if let Ok(Token::Op(op)) = &self.current_token {
                    if op != ")" {
                        return Err(format!("Expected ')' after while condition at line {}", self.lexer.line()));
                    }
                } else {
                    return Err(format!("Expected ')' after while condition at line {}", self.lexer.line()));
                }
                self.advance()?;
                let block = self.parse_block()?;
                Ok(Stmt::While(cond, block))
            }
            Ok(Token::Keyword(kw)) if kw == "return" => {
                self.advance()?;
                let expr = self.parse_expr()?;
                if let Ok(Token::Op(op)) = &self.current_token {
                    if op != ";" {
                        return Err(format!("Expected ';' after return at line {}", self.lexer.line()));
                    }
                } else {
                    return Err(format!("Expected ';' after return at line {}", self.lexer.line()));
                }
                self.advance()?;
                Ok(Stmt::Return(expr))
            }
            _ => {
                let expr = self.parse_expr()?;
                if let Ok(Token::Op(op)) = &self.current_token {
                    if op == ";" {
                        self.advance()?;
                        Ok(Stmt::Expr(expr))
                    } else if op == "=" {
                        self.advance()?;
                        let rhs = self.parse_expr()?;
                        if let Ok(Token::Op(op)) = &self.current_token {
                            if op != ";" {
                                return Err(format!("Expected ';' after assignment at line {}", self.lexer.line()));
                            }
                        } else {
                            return Err(format!("Expected ';' after assignment at line {}", self.lexer.line()));
                        }
                        self.advance()?;
                        match expr {
                            Expr::Var(id) => Ok(Stmt::Assign(id, rhs)),
                            _ => Err(format!("Expected variable for assignment at line {}", self.lexer.line())),
                        }
                    } else {
                        Err(format!("Expected '=' or ';' after expression at line {}", self.lexer.line()))
                    }
                } else {
                    Err(format!("Expected '=' or ';' after expression at line {}", self.lexer.line()))
                }
            }
        }
    }

    /// Parses an expression with additive operators (`+`, `-`).
    ///
    /// Handles expressions at the additive precedence level, delegating to `parse_term`
    /// for multiplication and division.
    ///
    /// # Returns
    /// * `Ok(Expr)` - The parsed expression.
    /// * `Err(String)` - An error message if the expression is invalid.
    pub fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_term()?;
        while let Ok(Token::Op(op)) = &self.current_token {
            if op != "+" && op != "-" {
                break;
            }
            let op = op.clone();
            self.advance()?;
            let right = self.parse_term()?;
            expr = Expr::BinOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    /// Parses a term with multiplicative operators (`*`, `/`).
    ///
    /// Handles expressions at the multiplicative precedence level, delegating to
    /// `parse_factor` for primary expressions.
    ///
    /// # Returns
    /// * `Ok(Expr)` - The parsed term.
    /// * `Err(String)` - An error message if the term is invalid.
    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_factor()?;
        while let Ok(Token::Op(op)) = &self.current_token {
            if op != "*" && op != "/" {
                break;
            }
            let op = op.clone();
            self.advance()?;
            let right = self.parse_factor()?;
            expr = Expr::BinOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    /// Parses a factor, including unary operators, literals, and function calls.
    ///
    /// Handles the highest precedence expressions, such as numbers, strings,
    /// variables, function calls, and parenthesized expressions.
    ///
    /// # Returns
    /// * `Ok(Expr)` - The parsed factor.
    /// * `Err(String)` - An error message if the factor is invalid.
    fn parse_factor(&mut self) -> Result<Expr, String> {
        if let Ok(Token::Op(op)) = &self.current_token {
            if op == "!" || op == "-" {
                let op = op.clone();
                self.advance()?;
                let expr = self.parse_factor()?;
                return Ok(Expr::UnaryOp(op, Box::new(expr)));
            }
        }

        match self.current_token.clone() {
            Ok(Token::Num(n)) => {
                self.advance()?;
                Ok(Expr::Num(n))
            }
            Ok(Token::String(s)) => {
                self.advance()?;
                Ok(Expr::String(s))
            }
            Ok(Token::Id(id)) => {
                self.advance()?;
                if let Ok(Token::Op(op)) = &self.current_token {
                    if op == "(" {
                        self.advance()?;
                        let args = self.parse_args()?;
                        match &self.current_token {
                            Ok(Token::Op(op)) if op == ")" => {
                                self.advance()?;
                                Ok(Expr::Call(id, args))
                            }
                            _ => Err(format!("Expected ')' at line {}", self.lexer.line())),
                        }
                    } else {
                        Ok(Expr::Var(id))
                    }
                } else {
                    Ok(Expr::Var(id))
                }
            }
            Ok(Token::Op(op)) if op == "(" => {
                self.advance()?;
                let expr = self.parse_expr()?;
                match &self.current_token {
                    Ok(Token::Op(op)) if op == ")" => {
                        self.advance()?;
                        Ok(expr)
                    }
                    _ => Err(format!("Expected ')' at line {}", self.lexer.line())),
                }
            }
            _ => Err(format!("Expected number, string, identifier, unary operator, or '(' at line {}", self.lexer.line())),
        }
    }

    /// Parses a comma-separated list of arguments for a function call.
    ///
    /// Handles zero or more expressions separated by commas, used in function calls.
    ///
    /// # Returns
    /// * `Ok(Vec<Expr>)` - The list of parsed arguments.
    /// * `Err(String)` - An error message if the arguments are invalid.
    fn parse_args(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();
        if let Ok(Token::Op(op)) = &self.current_token {
            if op == ")" {
                return Ok(args);
            }
        }
        args.push(self.parse_expr()?);
        while let Ok(Token::Op(op)) = &self.current_token {
            if op != "," {
                break;
            }
            self.advance()?;
            args.push(self.parse_expr()?);
        }
        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Lexer tests
    /// Tests recognition of C keywords.
    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("int if while return char");
        assert_eq!(lexer.next(), Ok(Token::Keyword("int".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Keyword("if".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Keyword("while".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Keyword("return".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Keyword("char".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests recognition of identifiers.
    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("main x123 printf");
        assert_eq!(lexer.next(), Ok(Token::Id("main".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Id("x123".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Id("printf".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests parsing of decimal numbers.
    #[test]
    fn test_number() {
        let mut lexer = Lexer::new("123 456");
        assert_eq!(lexer.next(), Ok(Token::Num(123)));
        assert_eq!(lexer.next(), Ok(Token::Num(456)));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests recognition of operators.
    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ == &&");
        assert_eq!(lexer.next(), Ok(Token::Op("+".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("==".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("&&".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests parsing of string literals.
    #[test]
    fn test_string() {
        let mut lexer = Lexer::new("\"hello\"");
        assert_eq!(lexer.next(), Ok(Token::String("hello".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests string literals with escape sequences.
    #[test]
    fn test_string_escape() {
        let mut lexer = Lexer::new("\"hello\\nworld\\\"\\\\\"");
        assert_eq!(lexer.next(), Ok(Token::String("hello\nworld\"\\".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests additional C operators.
    #[test]
    fn test_additional_operators() {
        let mut lexer = Lexer::new("<= >= != < > | & ! ~");
        assert_eq!(lexer.next(), Ok(Token::Op("<=".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op(">=".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("!=".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("<".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op(">".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("|".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("&".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("!".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("~".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests error handling for unclosed strings.
    #[test]
    fn test_invalid_string() {
        let mut lexer = Lexer::new("\"unclosed");
        assert!(lexer.next().is_err());
    }

    /// Tests skipping of single-line comments.
    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("// comment\nint x");
        assert_eq!(lexer.next(), Ok(Token::Keyword("int".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Id("x".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
        assert_eq!(lexer.line(), 2);
    }

    /// Tests parsing of hexadecimal numbers.
    #[test]
    fn test_hex_number() {
        let mut lexer = Lexer::new("0xFF 0x123");
        assert_eq!(lexer.next(), Ok(Token::Num(255)));
        assert_eq!(lexer.next(), Ok(Token::Num(291)));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests parsing of octal numbers.
    #[test]
    fn test_octal_number() {
        let mut lexer = Lexer::new("077 0123");
        assert_eq!(lexer.next(), Ok(Token::Num(63)));
        assert_eq!(lexer.next(), Ok(Token::Num(83)));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests recognition of punctuation tokens.
    #[test]
    fn test_punctuation() {
        let mut lexer = Lexer::new("(){};,");
        assert_eq!(lexer.next(), Ok(Token::Op("(".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op(")".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("{".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("}".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op(";".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op(",".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    /// Tests error handling for invalid characters.
    #[test]
    fn test_invalid_character() {
        let mut lexer = Lexer::new("@");
        assert_eq!(lexer.next(), Err("Unexpected character '@' at line 1".to_string()));
    }

    /// Tests recognition of preprocessor directives.
    #[test]
    fn test_directive() {
        let mut lexer = Lexer::new("#include <stdio.h>\n#define MAX 100\nint x");
        assert_eq!(lexer.next(), Ok(Token::Directive("include".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Directive("define".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Keyword("int".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Id("x".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
        assert_eq!(lexer.line(), 3);
    }

    /// Tests tokenization of a C4-like code snippet.
    #[test]
    fn test_c4_snippet() {
        let input = r#"
        #include <stdio.h>
        int main() {
            char *p = "hello\n";
            if (p != 0) {
                printf("%s", p);
                return 0xFF;
            }
            return 0;
        }
        "#;
        let mut lexer = Lexer::new(input);
        let expected = vec![
            Token::Directive("include".to_string()),
            Token::Keyword("int".to_string()),
            Token::Id("main".to_string()),
            Token::Op("(".to_string()),
            Token::Op(")".to_string()),
            Token::Op("{".to_string()),
            Token::Keyword("char".to_string()),
            Token::Op("*".to_string()),
            Token::Id("p".to_string()),
            Token::Op("=".to_string()),
            Token::String("hello\n".to_string()),
            Token::Op(";".to_string()),
            Token::Keyword("if".to_string()),
            Token::Op("(".to_string()),
            Token::Id("p".to_string()),
            Token::Op("!=".to_string()),
            Token::Num(0),
            Token::Op(")".to_string()),
            Token::Op("{".to_string()),
            Token::Id("printf".to_string()),
            Token::Op("(".to_string()),
            Token::String("%s".to_string()),
            Token::Op(",".to_string()),
            Token::Id("p".to_string()),
            Token::Op(")".to_string()),
            Token::Op(";".to_string()),
            Token::Keyword("return".to_string()),
            Token::Num(255),
            Token::Op(";".to_string()),
            Token::Op("}".to_string()),
            Token::Keyword("return".to_string()),
            Token::Num(0),
            Token::Op(";".to_string()),
            Token::Op("}".to_string()),
            Token::Eof,
        ];
        let mut tokens = Vec::new();
        while let Ok(token) = lexer.next() {
            tokens.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }
        assert_eq!(tokens, expected);
    }

    // Parser tests
    /// Tests parsing of a number literal.
    #[test]
    fn test_parse_number() {
        let mut parser = Parser::new("42");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::Num(42));
    }

    /// Tests parsing of a string literal.
    #[test]
    fn test_parse_string() {
        let mut parser = Parser::new("\"hello\"");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::String("hello".to_string()));
    }

    /// Tests parsing of a variable.
    #[test]
    fn test_parse_variable() {
        let mut parser = Parser::new("x");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::Var("x".to_string()));
    }

    /// Tests parsing of a function call.
    #[test]
    fn test_parse_function_call() {
        let mut parser = Parser::new("printf(\"%s\", p)");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::Call(
                "printf".to_string(),
                vec![
                    Expr::String("%s".to_string()),
                    Expr::Var("p".to_string())
                ]
            )
        );
    }

    /// Tests parsing of a unary minus operator.
    #[test]
    fn test_parse_unary_minus() {
        let mut parser = Parser::new("-x");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::UnaryOp("-".to_string(), Box::new(Expr::Var("x".to_string()))));
    }

    /// Tests parsing of a unary not operator.
    #[test]
    fn test_parse_unary_not() {
        let mut parser = Parser::new("!\"hello\"");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::UnaryOp("!".to_string(), Box::new(Expr::String("hello".to_string()))));
    }

    /// Tests parsing of a complex expression.
    #[test]
    fn test_parse_complex() {
        let mut parser = Parser::new("x + printf(\"%s\", p) * 2");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::Var("x".to_string())),
                "+".to_string(),
                Box::new(Expr::BinOp(
                    Box::new(Expr::Call(
                        "printf".to_string(),
                        vec![
                            Expr::String("%s".to_string()),
                            Expr::Var("p".to_string())
                        ]
                    )),
                    "*".to_string(),
                    Box::new(Expr::Num(2))
                ))
            )
        );
    }

    /// Tests parsing of an addition expression.
    #[test]
    fn test_parse_addition() {
        let mut parser = Parser::new("x + y");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::Var("x".to_string())),
                "+".to_string(),
                Box::new(Expr::Var("y".to_string()))
            )
        );
    }

    /// Tests operator precedence in expressions.
    #[test]
    fn test_parse_precedence() {
        let mut parser = Parser::new("x + y * 3");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::Var("x".to_string())),
                "+".to_string(),
                Box::new(Expr::BinOp(
                    Box::new(Expr::Var("y".to_string())),
                    "*".to_string(),
                    Box::new(Expr::Num(3))
                ))
            )
        );
    }

    /// Tests parsing with parentheses overriding precedence.
    #[test]
    fn test_parse_parentheses() {
        let mut parser = Parser::new("(x + y) * 3");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::BinOp(
                    Box::new(Expr::Var("x".to_string())),
                    "+".to_string(),
                    Box::new(Expr::Var("y".to_string()))
                )),
                "*".to_string(),
                Box::new(Expr::Num(3))
            )
        );
    }

    /// Tests parsing of an expression statement.
    #[test]
    fn test_parse_expr_stmt() {
        let mut parser = Parser::new("printf(\"%s\", p);");
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::Expr(Expr::Call(
                "printf".to_string(),
                vec![
                    Expr::String("%s".to_string()),
                    Expr::Var("p".to_string())
                ]
            ))
        );
    }

    /// Tests parsing of an assignment statement.
    #[test]
    fn test_parse_assign_stmt() {
        let mut parser = Parser::new("x = 5;");
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::Assign("x".to_string(), Expr::Num(5))
        );
    }

    /// Tests parsing of a return statement.
    #[test]
    fn test_parse_return_stmt() {
        let mut parser = Parser::new("return 0;");
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::Return(Expr::Num(0))
        );
    }

    /// Tests parsing of an if statement.
    #[test]
    fn test_parse_if_stmt() {
        let mut parser = Parser::new("if (x) { return 5; }");
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::If(
                Expr::Var("x".to_string()),
                vec![Stmt::Return(Expr::Num(5))],
                None
            )
        );
    }

    /// Tests parsing of an if-else statement.
    #[test]
    fn test_parse_if_else_stmt() {
        let mut parser = Parser::new("if (x) { return 5; } else { return 0; }");
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::If(
                Expr::Var("x".to_string()),
                vec![Stmt::Return(Expr::Num(5))],
                Some(vec![Stmt::Return(Expr::Num(0))])
            )
        );
    }

    /// Tests parsing of a while statement.
    #[test]
    fn test_parse_while_stmt() {
        let mut parser = Parser::new("while (x) { x = x + 1; }");
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::While(
                Expr::Var("x".to_string()),
                vec![Stmt::Assign(
                    "x".to_string(),
                    Expr::BinOp(
                        Box::new(Expr::Var("x".to_string())),
                        "+".to_string(),
                        Box::new(Expr::Num(1))
                    )
                )]
            )
        );
    }

    /// Tests parsing of a block of statements.
    #[test]
    fn test_parse_block() {
        let mut parser = Parser::new("{ x = 5; printf(\"%s\", p); return 0; }");
        let stmts = parser.parse_block().unwrap();
        assert_eq!(
            stmts,
            vec![
                Stmt::Assign("x".to_string(), Expr::Num(5)),
                Stmt::Expr(Expr::Call(
                    "printf".to_string(),
                    vec![
                        Expr::String("%s".to_string()),
                        Expr::Var("p".to_string())
                    ]
                )),
                Stmt::Return(Expr::Num(0))
            ]
        );
    }

    /// Tests error handling for an unclosed block.
    #[test]
    fn test_parse_error() {
        let mut parser = Parser::new("if (x) { return 5; ");
        let result = parser.parse_stmt();
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Expected '}' at end of block at line 1");
    }

    // Integration Tests
    /// Tests integration of parsing a block.
    #[test]
    fn test_integration_block() {
        let input = r#"
        {
            x = 5;
            printf("%s", p);
            return 0;
        }
        "#;
        let mut parser = Parser::new(input);
        let stmts = parser.parse_block().unwrap();
        assert_eq!(
            stmts,
            vec![
                Stmt::Assign("x".to_string(), Expr::Num(5)),
                Stmt::Expr(Expr::Call(
                    "printf".to_string(),
                    vec![
                        Expr::String("%s".to_string()),
                        Expr::Var("p".to_string())
                    ]
                )),
                Stmt::Return(Expr::Num(0))
            ]
        );
    }

    /// Tests integration of parsing an if-else statement.
    #[test]
    fn test_integration_if_else() {
        let input = r#"
        if (x) {
            return 5;
        } else {
            return 0;
        }
        "#;
        let mut parser = Parser::new(input);
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::If(
                Expr::Var("x".to_string()),
                vec![Stmt::Return(Expr::Num(5))],
                Some(vec![Stmt::Return(Expr::Num(0))])
            )
        );
    }

    /// Tests integration of parsing a while loop.
    #[test]
    fn test_integration_while() {
        let input = r#"
        while (x) {
            x = x + 1;
        }
        "#;
        let mut parser = Parser::new(input);
        let stmt = parser.parse_stmt().unwrap();
        assert_eq!(
            stmt,
            Stmt::While(
                Expr::Var("x".to_string()),
                vec![Stmt::Assign(
                    "x".to_string(),
                    Expr::BinOp(
                        Box::new(Expr::Var("x".to_string())),
                        "+".to_string(),
                        Box::new(Expr::Num(1))
                    )
                )]
            )
        );
    }
}