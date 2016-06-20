
use std::fmt;
use std::mem;

use twelve_bit::u12::*;

use assembler::source_file_location::SourceFileLocation;
use assembler::token::Token;
use assembler::token::display_names;

// MARK: - Abstract Syntax List

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
  /// A directive and corresponding parameters.
  Directive(DirectiveData<'a>),
  /// A label and corresponding parameters.
  Label(LabelData<'a>),
  /// An instruction and corresponding parameters.
  Instruction(InstructionData<'a>)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal<'a> {
  /// A literal string value.
  String(&'a str),
  /// A literal numeric value.
  Numeric(usize)
}

#[derive(Debug, PartialEq)]
pub struct DirectiveData<'a> {
  /// The location at which the directive is defined for error reporting purposes.
  pub location: SourceFileLocation<'a>,
  /// The name of the directive (db, org).
  pub identifier: &'a str,
  /// Arguments to the directive.
  pub arguments: Vec<Literal<'a>> 
}

#[derive(Debug, PartialEq)]
pub struct LabelData<'a> {
  /// The location at which the label is defined for error reporting purposes.
  pub location: SourceFileLocation<'a>,
  /// The name of the label.
  pub identifier: &'a str
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InstructionField<'a> {
  /// A numeric literal value.
  NumericLiteral(U12),
  /// A general-purpose register beginning with "v".
  GeneralPurposeRegister(u8),
  /// The delay timer register ("dt").
  DelayTimer,
  /// The sound timer register ("st").
  SoundTimer,
  /// The keypad register ("k").
  KeypadRegister,
  /// The index register ("i").
  IndexRegister,
  /// Register-indirect access of memory ("[i]").
  IndexRegisterIndirect,
  /// Any other literal identifier, usually a label.
  Identifier(&'a str)
}

#[derive(Debug, PartialEq)]
pub struct InstructionData<'a> {
  /// The location at which the directive is defined for error reporting purposes.
  pub location: SourceFileLocation<'a>,
  /// The mnemonic corresponding to the operation.
  pub mnemonic: &'a str,
  /// Instruction fields for the mnemonic.
  pub fields: Vec<InstructionField<'a>>
}

// MARK: - Local Parser Context

struct Parser<'a,I> where I: Iterator<Item=Token<'a>> {
  /// Iterator over input token stream.
  scanner: I,
  /// Current token.
  current_token: Option<Token<'a>>,
  /// Lookahead token.
  next_token: Option<Token<'a>>
}

// MARK: - Instruction Field

impl<'a> fmt::Display for InstructionField<'a> {
  /**
   Formats the instruction field for display purposes.
   */
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &InstructionField::NumericLiteral(value)      => write!(f, "${:x}", usize::from(value)),
      &InstructionField::GeneralPurposeRegister(r)  => write!(f, "v{:x}", r),
      &InstructionField::DelayTimer                 => write!(f, "dt"),
      &InstructionField::SoundTimer                 => write!(f, "st"),
      &InstructionField::KeypadRegister             => write!(f, "k"),
      &InstructionField::IndexRegister              => write!(f, "i"),
      &InstructionField::IndexRegisterIndirect      => write!(f, "[i]"),
      &InstructionField::Identifier(id)             => write!(f, "{}", id)
    }
  }
}

// MARK: - Helper Macros

/**
 Expects the presence of a token matching the given pattern. If found, the 
 token is consumed and the scanner advanced.
 @return The consumed token.
 */
macro_rules! expect_and_consume {
  ($parser:expr, $pattern:pat, $name:expr) => {{
    match $parser.current_token {
      $pattern => {
        $parser.consume_token().unwrap()
      }
      _ => { 
        return Err($parser.syntax_error_for_unexpected_token($name));
      }
    }
  }}
}

// MARK: - Parser Implementation

impl<'a,I> Parser<'a,I> where I: Iterator<Item=Token<'a>> {

  // MARK: - Initializers

  fn new(scanner: I) -> Self {
    let mut parser = Parser {
      scanner: scanner,
      current_token: None,
      next_token: None
    };
    parser.prime();
    parser
  }

  // MARK: - Private Methods

  /**
   Stores the next two tokens from the scanner as the current and next tokens
   respectively. Used to initialize the parser.
   */
  fn prime(&mut self) {
    self.current_token = self.scanner.next();
    self.next_token = self.scanner.next();
  }

  /**
   Consumes the current token, puts the next token in its place and advances
   the scanner. Returns the current token.
   @return The consumed token.
   */
  fn consume_token(&mut self) -> Option<Token<'a>> {
    let consumed_token = self.current_token.take();
    self.current_token = mem::replace(&mut self.next_token, self.scanner.next());
    consumed_token
  }

  /**
   @return A string describing the situation where the parse rule was expecting
     a certain token, and either a different one was found or the end of the
     token stream was reached prematurely.
   */
  fn syntax_error_for_unexpected_token(&self, expecting: &str) -> String {
    if let Some(ref token) = self.current_token {
      format!("{}: error: Unexpected token found: {}, expecting {}.", token.location(), token, expecting)
    } else {
      format!("error: Unexpected end-of-file reached, expecting {}.", expecting)
    }
  }

  // MARK: - Parser Rules

  /**
   Parses and consumes an IDENTIFIER token.
   identifier ::= IDENTIFIER.
   @return The string value of the identifier if successful, or an error.
   */
  fn parse_identifier(&mut self) -> Result<(&'a str, SourceFileLocation<'a>), String> {
    if let Token::Identifier(id,loc) = expect_and_consume!(self, Some(Token::Identifier(_,_)), display_names::IDENTIFIER) {
      Ok((id, loc))
    } else {
      unreachable!()
    }
  }

  /**
   Parses and consumes a string or numeric literal.
   literal ::= STRING | NUMERIC.
   @return The literal if successful, or an error.
   */
  fn parse_literal(&mut self) -> Result<Literal<'a>, String> {
    match self.current_token {

      // Match and consume a string literal token.
      Some(Token::StringLiteral(string, _)) => {
        let _ = self.consume_token();
        Ok(Literal::String(string))
      }

      // Match and consume a numeric literal token.
      Some(Token::NumericLiteral(number, _)) => {
        let _ = self.consume_token();
        Ok(Literal::Numeric(number))
      }

      _ => {
        let expected = format!("{} or {}", display_names::NUMERIC_LITERAL, display_names::STRING_LITERAL);
        Err(self.syntax_error_for_unexpected_token(expected.as_str()))
      }
    }
  }

  /**
   Parses and consumes a comma-separated list of zero-or-more string or numeric literals.
   literal_list ::= literal | literal "," literal_list | .
   @param list Provide an empty Vec<Literal>.
   @return A list of literals if successful, or an error.
   */
  fn parse_literal_list(&mut self, list: Vec<Literal<'a>>) -> Result<Vec<Literal<'a>>, String> {
    
    // The literal list is complete.
    if let Some(Token::Newline(_)) = self.current_token {
      return Ok(list);
    }

    // Parse the literal and append it to the list.
    let literal = try!(self.parse_literal());
    let mut result_list = list;
    result_list.push(literal);

    // A comma following the literal causes the rule to recurse.
    if let Some(Token::Comma(_)) = self.current_token {
      let _ = self.consume_token();
      result_list = try!(self.parse_literal_list(result_list));
    }
    
    // Return the complete list.
    Ok(result_list)
  }

  /**
   Parses and consumes an assembler directive and the associated literal parameters.
   directive ::= "." IDENTIFIER literal_list
   @return A directive node if successful, or an error.
   */
  fn parse_directive(&mut self) -> Result<Node<'a>, String> {
    expect_and_consume!(self, Some(Token::DirectiveMarker(_)), display_names::DIRECTIVE_MARKER);

    let ( id,loc ) = try!(self.parse_identifier());
    let parameters = try!(self.parse_literal_list(Vec::new()));

    let node_data = DirectiveData { location: loc, identifier: id, arguments: parameters };
    Ok(Node::Directive(node_data))
  }

  /**
   Parses and consumes a label.
   label ::= IDENTIFIER ":"
   @return A Label node if successful, or an error.
   */
  fn parse_label(&mut self) -> Result<Node<'a>, String> {
    let (id, loc) = try!(self.parse_identifier());

    expect_and_consume!(self, Some(Token::LabelMarker(_)), display_names::LABEL_MARKER);

    let label_data = LabelData { location: loc, identifier: id };
    Ok(Node::Label(label_data))
  }

  /**
   Parses and consumes a list of zero-or-more instruction fields. These fields can be numeric literals,
   delay timer, sound timer, index register (direct or indirect), keypad register or a label.
   field_list ::= field | field "," field_list | .
   field ::= NUMERIC | IDENTIFIER
   @param list Provide an empty Vec<InstructionField<'a>>.
   @return A list of fields if successful, or an error.
   */
  fn parse_field_list(&mut self, list: Vec<InstructionField<'a>>) -> Result<Vec<InstructionField<'a>>, String> {

    // The field list is complete.
    if let Some(Token::Newline(_)) = self.current_token {
      return Ok(list);
    }

    let field = match self.current_token {

      // A numeric literal parameter was matched.
      Some(Token::NumericLiteral(number, _)) => {
        let literal = InstructionField::NumericLiteral(number.unchecked_into());
        let _ = self.consume_token();
        literal
      }

      // An identiier was matched, which is either a Register or a Label.
      Some(Token::Identifier(id,_)) => {
        let identifier_field = match id {

          // Match special-purpose registers.
           "dt" => InstructionField::DelayTimer,
           "st" => InstructionField::SoundTimer,
            "i" => InstructionField::IndexRegister,
          "[i]" => InstructionField::IndexRegisterIndirect,
            "k" => InstructionField::KeypadRegister,

          // Now it's a GPR or an Identifier.
          identifier_string @ _ => {
            let mut field = InstructionField::Identifier(identifier_string);

            // If the identifier matches ("v"[0-9a-fA-F]), it's a GPR.
            let id_length = identifier_string.chars().count();
            if (id_length == 2) && identifier_string.starts_with("v") {
              let numeric_portion = &identifier_string['v'.len_utf8() .. ];
              if let Some(value) = u8::from_str_radix(numeric_portion, 16).ok() {
                field = InstructionField::GeneralPurposeRegister(value);
              }
            }

            field
          }
        };

        // Consume the matched identifier token.
        let _ = self.consume_token();
        identifier_field
      }

      // No field discovered.
      _ => {
        return Err(self.syntax_error_for_unexpected_token("instruction field"))
      }

    };

    // Append the parsed field to the list.
    let mut result_list = list;
    result_list.push(field);

    // A comma following the field causes the rule to recurse.
    if let Some(Token::Comma(_)) = self.current_token {
      let _ = self.consume_token();
      result_list = try!(self.parse_field_list(result_list));
    }
    
    // Return the complete list.
    Ok(result_list)
  }

  /**
   Parses and consumes an instruction and it's associated fields.
   instruction ::= IDENTIFIER field_list
   @return An Instruction node if successful, or an error.
   */
  fn parse_instruction(&mut self) -> Result<Node<'a>, String> {
    let (mnemonic,loc) = try!(self.parse_identifier());
    let list_of_fields = try!(self.parse_field_list(Vec::new()));

    let data = InstructionData { location: loc, mnemonic: mnemonic, fields: list_of_fields };
    Ok(Node::Instruction(data))
  }

  /**
   Parses and consumes one of the three types of statements (a directive, label or instruction).
   statement ::= directive | label | instruction
   @return An appropriate Node if successful, or an error.
   */
  fn parse_statement(&mut self) -> Result<Node<'a>, String> {

    // A directive is identified by a directive marker.
    if let Some(Token::DirectiveMarker(_)) = self.current_token {
      let directive_node = try!(self.parse_directive());
      return Ok(directive_node);
    }

    // A label is identified by an id followed by a label marker.
    if let (&Some(Token::Identifier(_,_)), &Some(Token::LabelMarker(_))) = (&self.current_token, &self.next_token) {
      let label_node = try!(self.parse_label());
      return Ok(label_node);
    }

    // Any remaining identifiers should be treated as instructions.
    if let Some(Token::Identifier(_,_)) = self.current_token {
      let instruction_node = try!(self.parse_instruction());
      return Ok(instruction_node);
    }

    // Generate syntax error based on token display names.
    let expecting = format!("{} or {}", display_names::DIRECTIVE_MARKER, display_names::IDENTIFIER);
    Err(self.syntax_error_for_unexpected_token(expecting.as_str()))
  }

  /**
   Parses and consumes a statement list, and consumes comments and newlines as they arrive.
   statement_list ::= (statement | COMMENT | NEWLINE) statement_list | .
   @return A list of consumed nodes if successful, or an error.
   */
  fn parse_statement_list(&mut self) -> Result<Vec<Node<'a>>, String> {
    let mut node_list = Vec::new();

    while let Some(_) = self.current_token {
      match self.current_token {

        // A single-line comment or newline is consumed in place.
        Some(Token::SingleLineComment(_, _)) | Some(Token::Newline(_)) => {
          let _ = self.consume_token();
        }

        // Append the new node to the ASL.
        _ => {
          let node = try!(self.parse_statement());
          node_list.push(node);
        }

      }
    }

    Ok(node_list)
  }

}

// MARK: - Parse Function

/**
 Converts the input token stream into an abstract syntax list representing the totality
 of the assembly file. This result is then fed into semantic analysis and code generation.
 @param scanner The input token stream.
 @return A list of Nodes for further processing, or an error.
 */
pub fn parse<'a, I>(scanner: I) -> Result<Vec<Node<'a>>, String> where I: Iterator<Item=Token<'a>> {
  Parser::new(scanner).parse_statement_list()
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use twelve_bit::u12::*;
  
  use assembler::source_file_location::SourceFileLocation;
  use assembler::scanner::Scanner;

  use super::Parser;
  use super::{DirectiveData, InstructionData, LabelData};
  use super::{Literal, Node, InstructionField};

  #[test]
  fn test_parse_literal_list() {
    // A literal list expects to be terminated by a newline.
    let mut parser = Parser::new(Scanner::new("-", ""));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Err("error: Unexpected end-of-file reached, expecting numeric literal or string literal.".to_string()));

    // A literal list cannot contain a comma alone.
    parser = Parser::new(Scanner::new("-", ",\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Err(format!("-:1:1: error: Unexpected token found: comma, expecting numeric literal or string literal.")));

    // An empty literal list is valid.
    parser = Parser::new(Scanner::new("-", "\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![]));

    // A literal list can contain a single item.
    parser = Parser::new(Scanner::new("-", "1\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![Literal::Numeric(1)]));
    parser = Parser::new(Scanner::new("-", "\"Hello\"\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![Literal::String("Hello")]));

    // A literal list can end in a trailing comma.
    parser = Parser::new(Scanner::new("-", "1,\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![Literal::Numeric(1)]));

    // A literal can contain multiple items of the same type.
    parser = Parser::new(Scanner::new("-", "1,2,3\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Ok(vec![Literal::Numeric(1), Literal::Numeric(2), Literal::Numeric(3)]));

    // A literal can contain multiple items of the different types.
    parser = Parser::new(Scanner::new("-", "1,\"Hello\",3\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Ok(vec![Literal::Numeric(1), Literal::String("Hello"), Literal::Numeric(3)]));
  }

  #[test]
  fn test_parse_directive() {
    // Test an origin directive.
    let mut org_parser = Parser::new(Scanner::new("-", ".org $100\n"));
    let expected_org_data = DirectiveData { 
      location: SourceFileLocation::new("-", 1, 2, 3),
      identifier: "org", 
      arguments: vec![Literal::Numeric(0x100)] 
    };
    assert_eq!(org_parser.parse_directive(), Ok(Node::Directive(expected_org_data)));

    // Test a db directive.
    let mut db_parser = Parser::new(Scanner::new("-", ".db \"Hello, World!\", $0\n"));
    let expected_db_data = DirectiveData { 
      location: SourceFileLocation::new("-", 1, 2, 2),
      identifier: "db", 
      arguments: vec![Literal::String("Hello, World!"), Literal::Numeric(0)]
    };
    assert_eq!(db_parser.parse_directive(), Ok(Node::Directive(expected_db_data)));

    // Test a fictional argument-free directive.
    let mut test_parser = Parser::new(Scanner::new("-", ".test\n"));
    let expected_test_data = DirectiveData { 
      location: SourceFileLocation::new("-", 1, 2, 4),
      identifier: "test", 
      arguments: vec![]
    };
    assert_eq!(test_parser.parse_directive(), Ok(Node::Directive(expected_test_data)));
  }

  #[test]
  fn test_parse_label() {
    // Parse a label.
    let mut parser = Parser::new(Scanner::new("-", "a:\n"));
    let expected_label_data = LabelData { 
      location: SourceFileLocation::new("-", 1, 1, 1),
      identifier: "a" 
    };
    assert_eq!(parser.parse_label(), Ok(Node::Label(expected_label_data)));
  }

  #[test]
  fn test_parse_field_list() {
    // A field list expects to be terminated by a newline.
    let mut parser = Parser::new(Scanner::new("-", ""));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Err("error: Unexpected end-of-file reached, expecting instruction field.".to_string()));

    // A field list cannot contain a comma alone.
    parser = Parser::new(Scanner::new("-", ",\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Err(format!("-:1:1: error: Unexpected token found: comma, expecting instruction field.")));

    // A field list cannot contain a string literal.
    parser = Parser::new(Scanner::new("-", "\"Hello\"\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Err(format!("-:1:1-7: error: Unexpected token found: string literal (\"Hello\"), expecting instruction field.")));

    // An empty field list is valid.
    parser = Parser::new(Scanner::new("-", "\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![]));

    // A field list can contain a single item.
    parser = Parser::new(Scanner::new("-", "$00\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::NumericLiteral(u12![0])]));
    parser = Parser::new(Scanner::new("-", "v1\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::GeneralPurposeRegister(1)]));
    parser = Parser::new(Scanner::new("-", "dt\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::DelayTimer]));
    parser = Parser::new(Scanner::new("-", "st\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::SoundTimer]));
    parser = Parser::new(Scanner::new("-", "k\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::KeypadRegister]));
    parser = Parser::new(Scanner::new("-", "i\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::IndexRegister]));
    parser = Parser::new(Scanner::new("-", "[i]\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::IndexRegisterIndirect]));
    parser = Parser::new(Scanner::new("-", "LABEL\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::Identifier("LABEL")]));

    // A field list can end in a trailing comma.
    parser = Parser::new(Scanner::new("-", "$FF,\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::NumericLiteral(U12::from(255))]));

    // A field list can contain multiple items of the same type.
    parser = Parser::new(Scanner::new("-", "v0,vf\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Ok(vec![
        InstructionField::GeneralPurposeRegister(0), 
        InstructionField::GeneralPurposeRegister(15)
      ])
    );

    // A field list can contain multiple items of the different types.
    parser = Parser::new(Scanner::new("-", "v1,v2,$ff\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Ok(vec![
        InstructionField::GeneralPurposeRegister(1),
        InstructionField::GeneralPurposeRegister(2),
        InstructionField::NumericLiteral(U12::from(255))
      ])
    );
  }

  #[test]
  fn test_parse_instruction() {
    // Test an instruction with no fields.
    let mut parser = Parser::new(Scanner::new("-", "nop\n"));
    let mut expected_data = InstructionData {
      location: SourceFileLocation::new("-", 1, 1, 3),
      mnemonic: "nop",
      fields: vec![]
    };
    assert_eq!(parser.parse_instruction(), Ok(Node::Instruction(expected_data)));

    // Test an instruction with one address target.
    parser = Parser::new(Scanner::new("-", "jp $002\n"));
    expected_data = InstructionData {
      location: SourceFileLocation::new("-", 1, 1, 2),
      mnemonic: "jp",
      fields: vec![ InstructionField::NumericLiteral(U12::from(2)) ]
    };
    assert_eq!(parser.parse_instruction(), Ok(Node::Instruction(expected_data)));

    // Test an instruction with a heterogenous field list.
    parser = Parser::new(Scanner::new("-", "drw v1, v2, $fe\n"));
    expected_data = InstructionData {
      location: SourceFileLocation::new("-", 1, 1, 3),
      mnemonic: "drw", 
      fields: vec![
        InstructionField::GeneralPurposeRegister(1),
        InstructionField::GeneralPurposeRegister(2),
        InstructionField::NumericLiteral(U12::from(254))
      ]
    };
    assert_eq!(parser.parse_instruction(), Ok(Node::Instruction(expected_data)));
  }

}
