
use std::mem;

/**
 program ::= statement_list

 statement_list ::= 
    statement statement_list
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
use assembler::instructions::{Literal, InstructionField, Node};

struct Parser<'a,I> where I: Iterator<Item=Token<'a>> {
  /// Iterator over input token stream.
  scanner: I,
  /// Current token.
  current_token: Option<Token<'a>>,
  /// Lookahead token.
  next_token: Option<Token<'a>>
}

// MARK: - Helper Macros

/**
 Expects the presence of a token matching the given pattern. If found, the 
 token is consumed and the scanner advanced.
 @return The consumed token.
 */
macro_rules! expect_and_consume {
  ($parser:expr, $pattern:pat) => {{
    match $parser.current_token {
      $pattern => {
        $parser.consume_token().unwrap()
      }
      _ => { 
        return Err($parser.syntax_error_for_unexpected_token(stringify!($pattern))); 
      }
    }
  }}
}

// MARK: - Parser Implementation

impl<'a,I> Parser<'a,I> where I: Iterator<Item=Token<'a>> {

  // MARK: - Initializers

  fn new(scanner: I) -> Parser<'a, I> {
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
    let consumed_token = mem::replace(&mut self.current_token, None);
    self.current_token = mem::replace(&mut self.next_token, self.scanner.next());
    consumed_token
  }

  /**
   @return A string describing the situation where the parse rule was expecting
   a certain token, and either a different one was found or the end of the
   token stream was reached prematurely.
   */
  fn syntax_error_for_unexpected_token(&self, expecting: &'static str) -> String {
    if let Some(ref token) = self.current_token {
      format!("Unexpected token found ({:?}), expecting {}.", token, expecting)
    } else {
      format!("Unexpected end-of-file reached. Expecting {}.", expecting)
    }
  } 

  // MARK: - Parser Rules

  fn parse_identifier(&mut self) -> Result<&'a str, String> {
    if let Token::Identifier(identifier) = expect_and_consume!(self, Some(Token::Identifier(_))) {
      Ok(identifier)
    } else {
      Err(format!("Internal parser error, failure of expect_and_consume! macro."))
    }
  }

  fn parse_literal(&mut self) -> Result<Literal<'a>, String> {
    // String literal.
    if let Some(Token::StringLiteral(string)) = self.current_token {
      let literal = Literal::String(string);
      let _ = self.consume_token();
      return Ok(literal);
    }

    // Numeric literal.
    if let Some(Token::NumericLiteral(number)) = self.current_token {
      let literal = Literal::Numeric(number);
      let _ = self.consume_token();
      return Ok(literal);
    }

    Err(self.syntax_error_for_unexpected_token("Numeric or String Literal"))
  }

  fn parse_literal_list(&mut self, list: Vec<Literal<'a>>) -> Result<Vec<Literal<'a>>, String> {
    /*
     literal_list ::= <literal>
     literal_list ::= <literal> "," <literal_list>
     literal_list ::= .
     */

    // The literal list is complete.
    if let Some(Token::Newline) = self.current_token {
      return Ok(list);
    }

    // Parse the literal and append it to the list.
    let literal = try!(self.parse_literal());
    let mut result_list = list;
    result_list.push(literal);

    // A comma following the literal causes the rule to recurse.
    if let Some(Token::Comma) = self.current_token {
      let _ = self.consume_token();
      result_list = try!(self.parse_literal_list(result_list));
    }
    
    // Return the complete list.
    Ok(result_list)
  }

  fn parse_directive(&mut self) -> Result<Node<'a>, String> {
    /*
     directive ::= "." <identifier> <literal_list>
     */
    expect_and_consume!(self, Some(Token::DirectiveMarker));

    let identifier = try!(self.parse_identifier());
    let parameters = try!(self.parse_literal_list(Vec::new()));

    Ok(Node::Directive { identifier: identifier, arguments: parameters})
  }

  fn parse_label(&mut self) -> Result<Node<'a>, String> {
    /*
     label ::= <identifier> ":"
     */

    let identifier = try!(self.parse_identifier());

    expect_and_consume!(self, Some(Token::LabelMarker));

    Ok(Node::Label {identifier: identifier})
  }

  fn parse_field_list(&mut self, list: Vec<InstructionField<'a>>) -> Result<Vec<InstructionField<'a>>, String> {
    /*
     field_list ::= <field>
     field_list ::= <field> "," <field_list>
     field_list ::= .
     */
    
    // The field list is complete.
    if let Some(Token::Newline) = self.current_token {
      return Ok(list);
    }

    let field = match self.current_token {

      // A numeric literal parameter was matched.
      Some(Token::NumericLiteral(number)) => {
        let literal = InstructionField::NumericLiteral(number);
        let _ = self.consume_token();
        literal
      }

      // An identiier was matched, which is either a Register or a Label.
      Some(Token::Identifier(id)) => {
        let identifier_field = match id {

          // Match special-purpose registers.
          "dt" => InstructionField::DelayTimer,
          "st" => InstructionField::SoundTimer,
           "i" => InstructionField::IndexRegister,
           "k" => InstructionField::KeypadRegister,

          // Now it's a GPR or an Identifier.
          identifier_string @ _ => {
            let mut field = InstructionField::Identifier(identifier_string);

            // If the identifier matches ("v"[0-9a-fA-F]), it's a GPR.
            let id_length = identifier_string.chars().count();
            if (id_length == 2) && identifier_string.chars().next().unwrap() == 'v' {
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
        return Err(self.syntax_error_for_unexpected_token("Instruction Field"))
      }

    };

    // Append the parsed field to the list.
    let mut result_list = list;
    result_list.push(field);

    // A comma following the field causes the rule to recurse.
    if let Some(Token::Comma) = self.current_token {
      let _ = self.consume_token();
      result_list = try!(self.parse_field_list(result_list));
    }
    
    // Return the complete list.
    Ok(result_list)
  }

  fn parse_instruction(&mut self) -> Result<Node<'a>, String> {
    /*
     instruction ::= <identifier> <field_list>
     */
    let mnemonic = try!(self.parse_identifier());
    let fields = try!(self.parse_field_list(Vec::new()));

    Ok(Node::Instruction { mnemonic: mnemonic, fields: fields })
  }

  fn parse_statement(&mut self) -> Result<Node<'a>, String> {
    /*
     statement ::= directive | label | instruction
     */

    // A directive is identified by a directive marker.
    if let Some(Token::DirectiveMarker) = self.current_token {
      let directive_node = try!(self.parse_directive());
      return Ok(directive_node);
    }

    // A label is identified by an id followed by a label marker.
    if let (&Some(Token::Identifier(_)), &Some(Token::LabelMarker)) = (&self.current_token, &self.next_token) {
      let label_node = try!(self.parse_label());
      return Ok(label_node);
    }

    // Any remaining identifiers should be treated as instructions.
    if let Some(Token::Identifier(_)) = self.current_token {
      let instruction_node = try!(self.parse_instruction());
      return Ok(instruction_node);
    }

    Err(format!("Failed to parse statement."))
  }

  fn parse_statement_list(&mut self) -> Result<Vec<Node<'a>>, String> {
    /*
     statement_list ::= statement statement_list | .
     */
    let mut node_list = Vec::new();

    while let Some(_) = self.current_token {
      match self.current_token {

        // A single-line comment or newline is consumed in place.
        Some(Token::SingleLineComment(_)) | Some(Token::Newline) => {
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

  // MARK: - Public Methods

  fn parse(&mut self) -> Result<Vec<Node<'a>>, String> {
    self.parse_statement_list()
  }

}

// MARK: - Parse Function

pub fn parse<'a, I>(scanner: I) -> Result<Vec<Node<'a>>, String> where I: Iterator<Item=Token<'a>> {
  let mut parser = Parser::new(scanner);
  parser.parse()
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use super::Parser;

  use assembler::scanner::Scanner;
  use assembler::instructions::{Literal, Node, InstructionField};

  #[test]
  fn test_parse_literal_list() {
    // A literal list expects to be terminated by a newline.
    let mut parser = Parser::new(Scanner::new(""));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Err("Unexpected end-of-file reached. Expecting Numeric or String Literal.".to_string()));

    // A literal list cannot contain a comma alone.
    parser = Parser::new(Scanner::new(",\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Err("Unexpected token found (Comma), expecting Numeric or String Literal.".to_string()));

    // An empty literal list is valid.
    parser = Parser::new(Scanner::new("\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![]));

    // A literal list can contain a single item.
    parser = Parser::new(Scanner::new("1\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![Literal::Numeric(1)]));
    parser = Parser::new(Scanner::new("\"Hello\"\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![Literal::String("Hello")]));

    // A literal list can end in a trailing comma.
    parser = Parser::new(Scanner::new("1,\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), Ok(vec![Literal::Numeric(1)]));

    // A literal can contain multiple items of the same type.
    parser = Parser::new(Scanner::new("1,2,3\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Ok(vec![Literal::Numeric(1), Literal::Numeric(2), Literal::Numeric(3)]));

    // A literal can contain multiple items of the different types.
    parser = Parser::new(Scanner::new("1,\"Hello\",3\n"));
    assert_eq!(parser.parse_literal_list(Vec::new()), 
      Ok(vec![Literal::Numeric(1), Literal::String("Hello"), Literal::Numeric(3)]));
  }

  #[test]
  fn test_parse_directive() {
    // Test an origin directive.
    let mut org_parser = Parser::new(Scanner::new(".org $100\n"));
    let expected_org = Node::Directive { identifier: "org", arguments: vec![Literal::Numeric(0x100)] };
    assert_eq!(org_parser.parse_directive(), Ok(expected_org));

    // Test a db directive.
    let mut db_parser = Parser::new(Scanner::new(".db \"Hello, World!\", $0\n"));
    let expected_db = Node::Directive { identifier: "db", arguments: vec![Literal::String("Hello, World!"), Literal::Numeric(0)] };
    assert_eq!(db_parser.parse_directive(), Ok(expected_db));

    // Test a fictional argument-free directive.
    let mut test_parser = Parser::new(Scanner::new(".test\n"));
    let expected_test = Node::Directive { identifier: "test", arguments: vec![] };
    assert_eq!(test_parser.parse_directive(), Ok(expected_test));
  }

  #[test]
  fn test_parse_label() {
    // Parse a label.
    let mut parser = Parser::new(Scanner::new("a:\n"));
    let expected_label = Node::Label { identifier: "a" }; 
    assert_eq!(parser.parse_label(), Ok(expected_label));
  }

  #[test]
  fn test_parse_field_list() {
    // A field list expects to be terminated by a newline.
    let mut parser = Parser::new(Scanner::new(""));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Err("Unexpected end-of-file reached. Expecting Instruction Field.".to_string()));

    // A field list cannot contain a comma alone.
    parser = Parser::new(Scanner::new(",\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Err("Unexpected token found (Comma), expecting Instruction Field.".to_string()));

    // A field list cannot contain a string literal.
    parser = Parser::new(Scanner::new("\"Hello\"\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Err("Unexpected token found (StringLiteral(\"Hello\")), expecting Instruction Field.".to_string()));

    // An empty field list is valid.
    parser = Parser::new(Scanner::new("\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![]));

    // A field list can contain a single item.
    parser = Parser::new(Scanner::new("$00\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::NumericLiteral(0)]));
    parser = Parser::new(Scanner::new("v1\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::GeneralPurposeRegister(1)]));
    parser = Parser::new(Scanner::new("dt\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::DelayTimer]));
    parser = Parser::new(Scanner::new("st\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::SoundTimer]));
    parser = Parser::new(Scanner::new("k\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::KeypadRegister]));
    parser = Parser::new(Scanner::new("i\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::IndexRegister]));
    parser = Parser::new(Scanner::new("LABEL\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::Identifier("LABEL")]));
    // TODO: Add a test for Index Register Indirect mode ([i]).

    // A field list can end in a trailing comma.
    parser = Parser::new(Scanner::new("$FF,\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), Ok(vec![InstructionField::NumericLiteral(255)]));

    // A field list can contain multiple items of the same type.
    parser = Parser::new(Scanner::new("v0,vf\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Ok(vec![
        InstructionField::GeneralPurposeRegister(0), 
        InstructionField::GeneralPurposeRegister(15)
      ])
    );

    // A field list can contain multiple items of the different types.
    parser = Parser::new(Scanner::new("v1,v2,$ff\n"));
    assert_eq!(parser.parse_field_list(Vec::new()), 
      Ok(vec![
        InstructionField::GeneralPurposeRegister(1),
        InstructionField::GeneralPurposeRegister(2),
        InstructionField::NumericLiteral(255)
      ])
    );
  }

  #[test]
  fn test_parse_instruction() {
    // Test an instruction with no fields.
    let mut parser = Parser::new(Scanner::new("nop\n"));
    let mut expected_instruction = Node::Instruction { mnemonic: "nop", fields: vec![] };
    assert_eq!(parser.parse_instruction(), Ok(expected_instruction));

    // Test an instruction with one address target.
    parser = Parser::new(Scanner::new("jp $002\n"));
    expected_instruction = Node::Instruction { mnemonic: "jp", fields: vec![ InstructionField::NumericLiteral(2) ]};
    assert_eq!(parser.parse_instruction(), Ok(expected_instruction));

    // Test an instruction with a heterogenous field list.
    parser = Parser::new(Scanner::new("drw v1, v2, $fe\n"));
    expected_instruction = Node::Instruction { mnemonic: "drw", fields: vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::NumericLiteral(254)
    ]};
    assert_eq!(parser.parse_instruction(), Ok(expected_instruction));
  }

}
