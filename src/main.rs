// src/main.rs
mod c4;

use c4::{Lexer, Token};
use std::env;
use std::fs;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(format!("Usage: {} <source.c>", args[0]));
    }

    let input = fs::read_to_string(&args[1]).map_err(|e| format!("Failed to read {}: {}", args[1], e))?;

    let mut lexer = Lexer::new(&input);

    loop {
        match lexer.next() {
            Some(Token::Eof) => break,
            Some(token) => println!("{:?}", token),
            None => {
                return Err(format!("Invalid token at line {}", lexer.line()));
            }
        }
    }

    Ok(())
}