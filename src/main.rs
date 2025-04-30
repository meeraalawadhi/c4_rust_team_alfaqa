// src/main.rs
mod c4;

use c4::{Lexer, Token};
use std::env;
use std::fs;

fn main() -> Result<(), String> {
    // Get command-line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(format!("Usage: {} <source.c>", args[0]));
    }

    // Read input file
    let input = fs::read_to_string(&args[1]).map_err(|e| format!("Failed to read {}: {}", args[1], e))?;

    // Initialize lexer
    let mut lexer = Lexer::new(&input);

    // Process tokens until EOF or error
    loop {
        match lexer.next() {
            Ok(Token::Eof) => break,
            Ok(token) => println!("{:?}", token),
            Err(e) => return Err(e),
        }
    }

    Ok(())
}