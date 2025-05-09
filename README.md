﻿# C4 Rust Compiler

A Rust implementation of the C4 compiler, a minimal C compiler and interpreter originally written in C by Robert Swierczek. This project rewrites the C4 compiler in Rust, maintaining its self-hosting capability and compatibility with the same subset of C code (e.g., the original C4 source). The project leverages Rust's safety features, such as ownership and pattern matching, to enhance design while preserving functional equivalence.

This is a university project by **Team Alfaqa**, consisting of:
- Meera Al Awadhi ([meeraalawadhi](https://github.com/meeraalawadhi))
- Hamda ([hamdathelamda](https://github.com/hamdathelamda))

**Repository**: [github.com/meeraalawadhi/c4_rust_team_alfaqa](https://github.com/meeraalawadhi/c4_rust_team_alfaqa)

**Status**: Work in progress (lexer implementation with basic tokens).

## Project Overview

The C4 Rust Compiler aims to:
- Compile the same subset of C code as the original C4, including self-hosting.
- Use Rust idioms (e.g., ownership, `Result`/`Option`) for safety and clarity.
- Include unit tests to verify compatibility with C4’s behavior.
- Provide documentation and a comparison report with the C implementation.

The project is developed collaboratively using GitHub, with contributions from both team members tracked via commits, branches, and pull requests.

## Prerequisites

- **Rust**: Install via [rustup](https://www.rust-lang.org/tools/install) (`rustc` and `cargo` required).
  - Verify: `rustc --version` (recommended: 1.75.0 or later, edition 2021).
- **Git**: For cloning the repository.
  - Verify: `git --version`.
- **Operating System**: Tested on Windows, macOS, and Linux.

## Current Progress
## Features
- **Lexer**:
  - Tokens: Keywords (`int`, `if`, `while`, `return`, `char`, `else`), identifiers, numbers (decimal, hex, octal), operators (`+`, `-`, `*`, `/`, `=`, `==`, `!=`, etc.), strings, comments, punctuation, preprocessor directives.
  - Tested with snippets like `c4.c`.
- **Parser**:
  - Expressions: Arithmetic (`+`, `-`, `*`, `/`), unary operators (`!`, `-`), variables, function calls, strings, numbers.
  - Statements: Assignments (`x = 5;`), expression statements (`printf("%s", p);`), returns, `if`/`else`, `while` loops.
  - Blocks: Parses braced blocks `{ ... }`.
- **Integration Tests**:
  - Verifies lexer and parser on realistic C4 snippets (blocks, `if-else`, `while`, error cases).
- **Main Program**:
  - Reads input file (e.g., `test.c`) and prints the parsed AST.

## Progress
- **Completed**:
  - Lexer for all required tokens.
  - Parser for expressions, statements, and control flow.
  - Integration tests for end-to-end parsing.
  - File input and error handling in `main.rs`.
  - Tested with `c4.c` and custom snippets.

## Installation

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/meeraalawadhi/c4_rust_team_alfaqa.git
   cd c4_rust_team_alfaqa
