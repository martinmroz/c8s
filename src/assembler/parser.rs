
use std::ascii::AsciiExt;
use std::iter::Peekable;
use std::collections::BTreeMap;

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

 instruction ::= <identifier> instruction_args_for(instruction)
 */

use assembler::scanner::Token;

// MARK: - Helper Macros

macro_rules! peek {
    ($scanner:expr,$p:pat) => {{
        if let Some(token) = $scanner.peek() {
            match *token {
                $p => { true  }
                 _ => { false }
            }
        } else {
            false
        }
    }}
}

macro_rules! accept {
    ($scanner:expr,$p:pat) => {{
        if peek!($scanner, $p) { 
            (true, $scanner.next()) 
        } else {
            (false, None)
        }
    }}
}

// MARK: - Type Definitions

type ParseResult = Result<(), String>;

// MARK: - Parser Object

struct Parser<'a,I> where I: Iterator<Item=Token<'a>> {
    /// Peekable iterator over input token stream.
    scanner: Peekable<I>,
    /// Stack of value-type tokens (Identifier, StringLiteral, NumericLiteral).
    value_stack: Vec<Token<'a>>,
    /// A map of labels and their corresponding addresses.
    label_map: BTreeMap<&'a str, usize>,
    /// Current address.
    current_address: usize
}

impl<'a,I> Parser<'a,I> where I: Iterator<Item=Token<'a>> {

    fn new(scanner: I) -> Parser<'a, I> {
        Parser {
            scanner: scanner.peekable(),
            value_stack: Vec::new(),
            label_map: BTreeMap::new(),
            current_address: 0
        }
    }

    /// MARK: - Expecting values.

    fn accept_identifier(&mut self) -> Option<&'a str> {
        if let (true, Some(Token::Identifier(string))) = accept!(self.scanner, Token::Identifier(_)) {
            Some(string)
        } else {
            None
        }
    }

    fn accept_string(&mut self) -> Option<&'a str> {
        if let (true, Some(Token::StringLiteral(string))) = accept!(self.scanner, Token::StringLiteral(_)) {
            Some(string)
        } else {
            None
        }
    }

    fn accept_numeric(&mut self) -> Option<usize> {
        if let (true, Some(Token::NumericLiteral(number))) = accept!(self.scanner, Token::NumericLiteral(_)) {
            Some(number)
        } else {
            None
        }
    }

    /// MARK: - Non-terminal Symbols.

    pub fn parse(&mut self) -> ParseResult {
        /*
         statement_list ::= statement | statement_list | .
         */
        while let Some(_) = self.scanner.peek() {
            try!(self.parse_statement());
        }
        Ok(())
    }

    fn parse_statement(&mut self) -> ParseResult {
        /*
         statement ::= comment | directive | label | instruction
         */

        // Consume the single-line comment token as it appears.
        if let (true, _) = accept!(self.scanner, Token::SingleLineComment(_)) {
            return Ok(());
        }

        // A directive begins with a directive marker token.
        if peek!(self.scanner, Token::DirectiveMarker) {
            return self.parse_directive();
        }

        if let Some(identifier) = self.accept_identifier() {
            /*
                   label ::= <identifier> ":"
             instruction ::= <identifier> instruction_args_for(instruction)
             */

            // Both 'instruction' and 'label' begin with identifiers, and label is proceeded by a LabelIdentifier.
            if let (true, _) = accept!(self.scanner, Token::LabelMarker) {
                return self.interpret_label(identifier);
            }

            // This must, therefore, be an instruction.
            return self.parse_instruction_args_with_identifier(identifier);
        }

        Err(format!("Unable to parse statement"))
    }

    fn parse_directive(&mut self) -> ParseResult {
        /*
         directive ::= <identifier> (<string_literal> | <numeric_literal>)
         */
        
        // Expect the presence of  directive marker.
        if let (false, None) = accept!(self.scanner, Token::DirectiveMarker) {
            return Err(format!("Unexpected token {:?}. Expecting directive marker.", self.scanner.peek()));
        }

        // Expect the presence of an identifier.
        let directive = if let Some(identifier) = self.accept_identifier() {
            identifier
        } else {
            return Err(format!("Expected identifier after directive marker."));
        };

        // Expect either a numeric or a string literal, and interpret accordingly.
        if let Some(string) = self.accept_string() {
            self.interpret_directive_s(directive, string)
        } else if let Some(numeric) = self.accept_numeric() {
            self.interpret_directive_n(directive, numeric)
        } else {
            Err(format!("Expected literal (string or numeric) for directive .{}", directive))
        }
    }

    fn parse_instruction_args_with_identifier(&mut self, instruction: &'a str) -> ParseResult {
        /*
         instruction ::= <identifier> instruction_args_for(instruction)
         */
        Ok(())
    }

    /// MARK: - Interpreters.

    fn interpret_directive_s(&mut self, name: &'a str, string_arg: &'a str) -> ParseResult {
        println!("Directive<Type=String>('{}', '{}')", name, string_arg);

        // Emit a string literal.
        if name.eq_ignore_ascii_case("db") {
            return Ok(())
        }

        Err(format!("Unknown directive {} [Type=String] ('{}')", name, string_arg))
    }

    fn interpret_directive_n(&mut self, name: &'a str, numeric_arg: usize) -> ParseResult {
        println!("Directive<Type=Numeric>('{}', ${:X})", name, numeric_arg);

        // Set the current output address for the assembly.
        if name.eq_ignore_ascii_case("org") {
            self.current_address = numeric_arg;
            return Ok(())
        }

        // Emit a single byte numeric literal.
        if name.eq_ignore_ascii_case("db") {
            if (numeric_arg & 0xFF) != 0 {
                return Err(format!("Cannot emit byte with numeric value > 0xFF (was ${:X})", numeric_arg));
            } else {
                return Ok(());
            }
        }

        Err(format!("Unknown directive {} [Type=Numeric] (${:X})", name, numeric_arg))
    }

    fn interpret_label(&mut self, name: &'a str) -> ParseResult {
        // An attempt was made to re-define the label.
        if let Some(&existing_address) = self.label_map.get(name) {
            return Err(format!("Attempted re-definition of label '{}' (${:X}) to ${:X}", name, existing_address, self.current_address))
        }

        // Store the current address in the label map.
        println!("DefineLabel('{}') = ${:X}", name, self.current_address);
        self.label_map.insert(name, self.current_address);
        Ok(())
    }

}

// MARK: - Parse Function

pub fn parse<'a, I>(scanner: I) where I: Iterator<Item=Token<'a>> {
    let mut parser = Parser::new(scanner);
    println!("{:?}", parser.parse());
}
