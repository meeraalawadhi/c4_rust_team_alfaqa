// src/c4.rs
use std::collections::HashMap;

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

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: i32,
    keywords: HashMap<String, Token>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut keywords = HashMap::new();
        for kw in ["int", "if", "while", "return", "char"].iter() {
            keywords.insert(kw.to_string(), Token::Keyword(kw.to_string()));
        }
        Lexer {
            input: input.chars().collect(),
            pos: 0,
            line: 1,
            keywords,
        }
    }

    pub fn line(&self) -> i32 {
        self.line
    }

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

// Parser implementation
#[derive(Debug, PartialEq)]
pub enum Expr {
    Num(i64),
    String(String), // New: String literals
    UnaryOp(String, Box<Expr>), // New: Unary operators (!, -)
    BinOp(Box<Expr>, String, Box<Expr>),
}

pub struct Parser {
    lexer: Lexer,
    current_token: Result<Token, String>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next();
        Parser {
            lexer,
            current_token,
        }
    }

    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.lexer.next();
        Ok(())
    }

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

    fn parse_factor(&mut self) -> Result<Expr, String> {
        // Handle unary operators
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
            _ => Err(format!("Expected number, string, unary operator, or '(' at line {}", self.lexer.line())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Lexer tests
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

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("main x123 printf");
        assert_eq!(lexer.next(), Ok(Token::Id("main".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Id("x123".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Id("printf".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    #[test]
    fn test_number() {
        let mut lexer = Lexer::new("123 456");
        assert_eq!(lexer.next(), Ok(Token::Num(123)));
        assert_eq!(lexer.next(), Ok(Token::Num(456)));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ == &&");
        assert_eq!(lexer.next(), Ok(Token::Op("+".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("==".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Op("&&".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    #[test]
    fn test_string() {
        let mut lexer = Lexer::new("\"hello\"");
        assert_eq!(lexer.next(), Ok(Token::String("hello".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    #[test]
    fn test_string_escape() {
        let mut lexer = Lexer::new("\"hello\\nworld\\\"\\\\\"");
        assert_eq!(lexer.next(), Ok(Token::String("hello\nworld\"\\".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

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

    #[test]
    fn test_invalid_string() {
        let mut lexer = Lexer::new("\"unclosed");
        assert!(lexer.next().is_err());
    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("// comment\nint x");
        assert_eq!(lexer.next(), Ok(Token::Keyword("int".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Id("x".to_string())));
        assert_eq!(lexer.next(), Ok(Token::Eof));
        assert_eq!(lexer.line(), 2);
    }

    #[test]
    fn test_hex_number() {
        let mut lexer = Lexer::new("0xFF 0x123");
        assert_eq!(lexer.next(), Ok(Token::Num(255)));
        assert_eq!(lexer.next(), Ok(Token::Num(291)));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    #[test]
    fn test_octal_number() {
        let mut lexer = Lexer::new("077 0123");
        assert_eq!(lexer.next(), Ok(Token::Num(63)));
        assert_eq!(lexer.next(), Ok(Token::Num(83)));
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

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

    #[test]
    fn test_invalid_character() {
        let mut lexer = Lexer::new("@");
        assert_eq!(lexer.next(), Err("Unexpected character '@' at line 1".to_string()));
    }

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
    #[test]
    fn test_parse_number() {
        let mut parser = Parser::new("42");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::Num(42));
    }

    #[test]
    fn test_parse_string() {
        let mut parser = Parser::new("\"hello\"");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::String("hello".to_string()));
    }

    #[test]
    fn test_parse_unary_minus() {
        let mut parser = Parser::new("-5");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::UnaryOp("-".to_string(), Box::new(Expr::Num(5))));
    }

    #[test]
    fn test_parse_unary_not() {
        let mut parser = Parser::new("!\"hello\"");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::UnaryOp("!".to_string(), Box::new(Expr::String("hello".to_string()))));
    }

    #[test]
    fn test_parse_unary_complex() {
        let mut parser = Parser::new("-(2 + 3)");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::UnaryOp(
                "-".to_string(),
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(2)),
                    "+".to_string(),
                    Box::new(Expr::Num(3))
                ))
            )
        );
    }

    #[test]
    fn test_parse_addition() {
        let mut parser = Parser::new("1 + 2");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(Box::new(Expr::Num(1)), "+".to_string(), Box::new(Expr::Num(2)))
        );
    }

    #[test]
    fn test_parse_precedence() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::Num(1)),
                "+".to_string(),
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(2)),
                    "*".to_string(),
                    Box::new(Expr::Num(3))
                ))
            )
        );
    }

    
}