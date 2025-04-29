// src/c4.rs
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(i64),
    Id(String),
    Keyword(String),
    Op(String),
    String(String),
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

    pub fn line(&self) -> i32 {
        self.line
    }

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

        // Single-line comment
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

        // Identifier or keyword
        if c.is_alphabetic() || c == '_' {
            let start = self.pos - 1; // Renamed from _start
            while self.pos < self.input.len() && (self.input[self.pos].is_alphanumeric() || self.input[self.pos] == '_') {
                self.pos += 1;
            }
            let id = self.input[start..self.pos].iter().collect::<String>();
            return Some(self.keywords.get(&id).cloned().unwrap_or(Token::Id(id)));
        }

        // Number (decimal, hex, octal)
        if c.is_digit(10) {
            let start = self.pos - 1;
            let mut num_str = String::new();
            num_str.push(c);
            if c == '0' && self.pos < self.input.len() && self.input[self.pos].to_ascii_lowercase() == 'x' {
                num_str.push(self.input[self.pos]);
                self.pos += 1;
                while self.pos < self.input.len() && self.input[self.pos].is_digit(16) {
                    num_str.push(self.input[self.pos]);
                    self.pos += 1;
                }
                return Some(Token::Num(i64::from_str_radix(&num_str[2..], 16).unwrap_or(0)));
            } else if c == '0' && self.pos < self.input.len() && self.input[self.pos].is_digit(8) {
                while self.pos < self.input.len() && self.input[self.pos].is_digit(8) {
                    num_str.push(self.input[self.pos]);
                    self.pos += 1;
                }
                return Some(Token::Num(i64::from_str_radix(&num_str[1..], 8).unwrap_or(0)));
            } else {
                while self.pos < self.input.len() && self.input[self.pos].is_digit(10) {
                    num_str.push(self.input[self.pos]);
                    self.pos += 1;
                }
                return Some(Token::Num(num_str.parse().unwrap_or(0)));
            }
        }

        // Operators and punctuation
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
            return Some(Token::Op(op));
        }

        // String literal with escape sequences
        if c == '"' {
            let mut s = String::new();
            while self.pos < self.input.len() && self.input[self.pos] != '"' {
                if self.input[self.pos] == '\\' {
                    self.pos += 1;
                    if self.pos >= self.input.len() {
                        return None;
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
                return None;
            }
            self.pos += 1;
            return Some(Token::String(s));
        }

        // Unknown character
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

    #[test]
    fn test_string_escape() {
        let mut lexer = Lexer::new("\"hello\\nworld\\\"\\\\\"");
        assert_eq!(lexer.next(), Some(Token::String("hello\nworld\"\\".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_additional_operators() {
        let mut lexer = Lexer::new("<= >= != < > | & ! ~");
        assert_eq!(lexer.next(), Some(Token::Op("<=".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op(">=".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("!=".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("<".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op(">".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("|".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("&".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("!".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("~".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_invalid_string() {
        let mut lexer = Lexer::new("\"unclosed");
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("// comment\nint x");
        assert_eq!(lexer.next(), Some(Token::Keyword("int".to_string())));
        assert_eq!(lexer.next(), Some(Token::Id("x".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
        assert_eq!(lexer.line(), 2);
    }

    #[test]
    fn test_hex_number() {
        let mut lexer = Lexer::new("0xFF 0x123");
        assert_eq!(lexer.next(), Some(Token::Num(255)));
        assert_eq!(lexer.next(), Some(Token::Num(291)));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_octal_number() {
        let mut lexer = Lexer::new("077 0123");
        assert_eq!(lexer.next(), Some(Token::Num(63)));
        assert_eq!(lexer.next(), Some(Token::Num(83)));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }

    #[test]
    fn test_punctuation() {
        let mut lexer = Lexer::new("(){};,");
        assert_eq!(lexer.next(), Some(Token::Op("(".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op(")".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("{".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op("}".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op(";".to_string())));
        assert_eq!(lexer.next(), Some(Token::Op(",".to_string())));
        assert_eq!(lexer.next(), Some(Token::Eof));
    }
}