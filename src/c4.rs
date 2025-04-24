 
// src/c4.rs
use std::collections::HashMap;

// Token types, mirroring C4's enum
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(i64),           // Numeric literal (e.g., 123, 0xFF)
    Id(String),         // Identifier (e.g., main, x)
    Keyword(String),    // Keyword (e.g., int, if)
    Op(String),         // Operator (e.g., +, ==)
    String(String),     // String literal (e.g., "hello")
    Eof,                // End of input
}

// Lexer state
pub struct Lexer {
    input: Vec<char>,   // Input source code as char vector
    pos: usize,         // Current position in input
    line: i32, // Keep private (no `pub`)
    keywords: HashMap<String, Token>, // Keyword lookup table
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut keywords = HashMap::new();
        // Initialize keywords (subset of C4's keywords)
        for kw in ["int", "if", "while", "return"].iter() {
            keywords.insert(kw.to_string(), Token::Keyword(kw.to_string()));
        }
        Lexer {
            input: input.chars().collect(),
            pos: 0,
            line: 1,
            keywords,
        }
    }

    // Add getter method for line
    pub fn line(&self) -> i32 {
        self.line
    }

    // Get the next token
    pub fn next(&mut self) -> Option<Token> {
        // Skip whitespace and handle newlines
        while self.pos < self.input.len() && self.input[self.pos].is_whitespace() {
            if self.input[self.pos] == '\n' {
                self.line += 1;
            }
            self.pos += 1;
        }

        // Check for end of input
        if self.pos >= self.input.len() {
            return Some(Token::Eof);
        }

        let c = self.input[self.pos];
        self.pos += 1;

        // Identifier or keyword
        if c.is_alphabetic() || c == '_' {
            let start = self.pos - 1;
            while self.pos < self.input.len() && (self.input[self.pos].is_alphanumeric() || self.input[self.pos] == '_') {
                self.pos += 1;
            }
            let id = self.input[start..self.pos].iter().collect::<String>();
            return Some(self.keywords.get(&id).cloned().unwrap_or(Token::Id(id)));
        }

        // Number (decimal only for simplicity)
        if c.is_digit(10) {
            let start = self.pos - 1;
            while self.pos < self.input.len() && self.input[self.pos].is_digit(10) {
                self.pos += 1;
            }
            let num = self.input[start..self.pos].iter().collect::<String>();
            return Some(Token::Num(num.parse().unwrap_or(0)));
        }

        // Operators (e.g., +, ==)
        if "+-*/==&&||".contains(c) {
            let mut op = c.to_string();
            if self.pos < self.input.len() && "=&|".contains(c) && self.input[self.pos] == c {
                op.push(c);
                self.pos += 1;
            }
            return Some(Token::Op(op));
        }

        // String literal
        if c == '"' {
            let start = self.pos;
            while self.pos < self.input.len() && self.input[self.pos] != '"' {
                self.pos += 1;
            }
            if self.pos < self.input.len() {
                self.pos += 1; // Skip closing quote
                let s = self.input[start..self.pos-1].iter().collect::<String>();
                return Some(Token::String(s));
            }
        }

        // Unknown character (error or skip)
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("int if while return");
        assert_eq!(lexer.next(), Some(Token::Keyword("int".to_string())));
        assert_eq!(lexer.next(), Some(Token::Keyword("if".to_string())));
        assert_eq!(lexer.next(), Some(Token::Keyword("while".to_string())));
        assert_eq!(lexer.next(), Some(Token::Keyword("return".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("main x123");
        assert_eq!(lexer.next(), Some(Token::Id("main".to_string())));
        assert_eq!(lexer.next(), Some(Token::Id("x123".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_number() {
        let mut lexer = Lexer::new("123 456");
        assert_eq!(lexer.next(), Some(Token::Num(123)));
        assert_eq!(lexer.next(), Some(Token::Num(456)));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ == &&");
        assert_eq!(lexer.next(), Some(Token::Op("+".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("==".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("&&".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_string() {
        let mut lexer = Lexer::new("\"hello\"");
        assert_eq!(lexer.next(), Some(Token::String("hello".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }
}