// src/main.rs
mod c4;

use c4::Lexer;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <source.c>", args[0]);
        std::process::exit(1);
    }

    // Read input file (placeholder, to be expanded)
    let input = "int main() { return 0; }"; // Hardcoded for testing
    let mut lexer = Lexer::new(input);
    loop {
        match lexer.next() {
            Some(token) => println!("{:?}", token),
            None => {
                eprintln!("Invalid token at line {}", lexer.line()); // Use getter
                break;
            }
        }
        if let Some(c4::Token::Eof) = lexer.next() {
            break;
        }
    }
}