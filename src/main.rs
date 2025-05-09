mod c4;

use c4::Parser;
use std::env;
use std::fs;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(format!("Usage: {} <source.c>", args[0]));
    }

    let input = fs::read_to_string(&args[1]).map_err(|e| format!("Failed to read {}: {}", args[1], e))?;

    let mut parser = Parser::new(&input);
    let stmts = parser.parse_block()?;
    for stmt in stmts {
        println!("{:#?}", stmt);
    }

    Ok(())
}