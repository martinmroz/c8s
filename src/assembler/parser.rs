
use std::ascii::AsciiExt;
use std::iter::Peekable;

/**
 program ::= statement_list

 statement_list ::= 
    statement
    | statement_list
    | .

 statement ::= 
    directive
    | label
    | instruction
    | comment
 
 directive ::= "." <identifier> (<string_literal> | <numeric_literal>)

 label ::= <identifier> ":"
 */

use assembler::scanner::Token;

// MARK: - Instruction Formats

fn is_i0_instruction<'a>(instruction: &'a str) -> bool {
    match instruction.to_ascii_lowercase().as_str() {
        "nop" | "cls" | "ret" | "trap" | "trapret" => { true }
        _ => { false }
    }
}

fn is_iA_instruction<'a>(instruction: &'a str) -> bool {
    match instruction.to_ascii_lowercase().as_str() {
        "jp" | "call" => { true }
        _ => { false }
    }
}

// MARK: - Helper Macros

macro_rules! expect {
    ($scanner:expr, $expected:pat) => {{
        let token = $scanner.next().unwrap();
        match token {
            $expected => {}
            _ => {
                println!("Invalid token {:?}", token);
                return;
            }
        }
    }};
}

macro_rules! accept {
    ($scanner:expr, $expected:pat) => {{
        let mut accepted = false;
        if let Some(token) = $scanner.peek() {
            match token {
                $expected => { accepted = true; }
                _ => { }
            }
        }
        if accepted {
            $scanner.next();
        }
        accepted
    }}
}

// MARK: - State Handlers

fn syntax_error<'a, I>(scanner: &mut Peekable<I>) where I: Iterator<Item=Token<'a>> {
    /*
     An invalid token not matching a terminal or a non-terminal was reached.
     */
    println!("Unexpected {:?} found.", scanner.next().unwrap());
    println!("Terminating assembly.");
    scanner.last();
}

// MARK: - Helper Methods

fn accept_identifier<'a, I>(scanner: &mut Peekable<I>) -> Option<&'a str> where I: Iterator<Item=Token<'a>> {
    let mut value = None;
    if let Some(identifier_token) = scanner.peek() {
        match identifier_token {
            &Token::Identifier(string) => { value = Some(string); }
            _ => {}
        }
    }
    if let Some(_) = value {
        scanner.next();
    }
    value
}

fn accept_numeric_literal<'a, I>(scanner: &mut Peekable<I>) -> Option<usize> where I: Iterator<Item=Token<'a>> {
    let mut value = None;
    if let Some(numeric_literal_token) = scanner.peek() {
        match numeric_literal_token {
            &Token::NumericLiteral(numeric) => { value = Some(numeric); }
            _ => {}
        }
    }
    if let Some(_) = value {
        scanner.next();
    }
    value
}

fn accept_string_literal<'a, I>(scanner: &mut Peekable<I>) -> Option<&'a str> where I: Iterator<Item=Token<'a>> {
    let mut value = None;
    if let Some(string_literal_token) = scanner.peek() {
        match string_literal_token {
            &Token::StringLiteral(string) => { value = Some(string); }
            _ => {}
        }
    }
    if let Some(_) = value {
        scanner.next();
    }
    value
}

// MARK: - Rules

fn comment<'a, I>(scanner: &mut Peekable<I>) where I: Iterator<Item=Token<'a>> {
    expect!(scanner, Token::SingleLineComment(_));
}

fn label_or_instruction<'a, I>(scanner: &mut Peekable<I>) where I: Iterator<Item=Token<'a>> {
    let mut successful_evaluation = false;

    /*
     label ::= <identifier> ":"
     instruction ::= 
        [i0] (nop/cls/ret/trap/trapret)
        [iA] (jp/call)
     */

    let identifier = accept_identifier(scanner).unwrap();

    // Defines a new label at the current evaluation point.
    if accept!(scanner, &Token::LabelMarker) {
        println!("LABEL({})", identifier);
        return;
    }

    // These instructions do not accept parameters.
    if is_i0_instruction(identifier) {
        println!("INSTRUCTION({})", identifier);
        return;
    }

    // These instructions accept an address parameter.
    if is_iA_instruction(identifier) {
        if let Some(address) = accept_numeric_literal(scanner) {
            println!("INSTRUCTION({} {})", identifier, address);
            return;
        }
    }

    // The subsequent tokens did not match the rule.
    if !successful_evaluation {
        syntax_error(scanner);
    }
}

fn directive<'a, I>(scanner: &mut Peekable<I>) where I: Iterator<Item=Token<'a>> {
    let mut successful_evaluation = false;

    /*
     directive ::= "." <identifier> (<string_literal> | <numeric_literal>)
     */
    expect!(scanner, Token::DirectiveMarker);

    // Extract the directive ID if possible.
    if let Some(identifier) = accept_identifier(scanner) {
        if let Some(numeric_literal) = accept_numeric_literal(scanner) {
            println!("Directive({}) Numeric({})", identifier, numeric_literal);
            successful_evaluation = true;
        }

        if let Some(string_literal) = accept_string_literal(scanner) {
            println!("Directive({}) String({})", identifier, string_literal);
            successful_evaluation = true;
        }
    }

    // The subsequent tokens did not match a directive declaration.
    if !successful_evaluation {
        syntax_error(scanner);
    }
}

fn statement<'a, I>(scanner: &mut Peekable<I>) where I: Iterator<Item=Token<'a>> {
    /*
     statement ::= comment | directive | label | instruction
     */
    let mut next_rule: fn(&mut Peekable<I>) -> () = syntax_error;

    // Continue parsing the input file until it yields no more tokens.
    if let Some(next_token) = scanner.peek() {
        next_rule = match next_token {
            &Token::SingleLineComment(_) => { comment }
            &Token::DirectiveMarker => { directive }
            &Token::Identifier(_) => { label_or_instruction }
            _ => { syntax_error }
        };
    }

    next_rule(scanner);
}

fn statement_list<'a, I>(scanner: &mut Peekable<I>) where I: Iterator<Item=Token<'a>> {
    /*
     statement_list ::= statement_list statement | statement | .
     */
    while let Some(_) = scanner.peek() {
        statement(scanner);
    }
}

pub fn parse<'a, I>(scanner: I) where I: Iterator<Item=Token<'a>> {
    /*
     program ::= statement_list
     */
    statement_list(&mut scanner.peekable());
}
