
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
use assembler::instructions::Literal;
use assembler::instructions::{DirectiveNode, LabelNode};

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
macro_rules! expect {
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
    if let Token::Identifier(identifier) = expect!(self, Some(Token::Identifier(_))) {
      Ok(identifier)
    } else {
      Err(format!("Internal parser error, failure of expect! macro."))
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
     literal_list ::= <literal_list> "," <literal>
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

  fn parse_directive(&mut self) -> Result<DirectiveNode, String> {
    /*
     directive ::= "." <identifier> <literal_list>
     */
    expect!(self, Some(Token::DirectiveMarker));

    let identifier = try!(self.parse_identifier());
    let parameters = try!(self.parse_literal_list(Vec::new()));
    let node = DirectiveNode::new(identifier, parameters);

    println!("{:?}", &node);

    Ok(node)
  }

  fn parse_label(&mut self) -> Result<LabelNode, String> {
    /*
     label ::= <identifier> ":"
     */

    let identifier = try!(self.parse_identifier());
    let node = LabelNode::new(identifier);

    expect!(self, Some(Token::LabelMarker));

    println!("{:?}", &node);

    Ok(node)
  }

  fn parse_instruction(&mut self) -> Result<(),String> {
    /*
     instruction ::= <identifier> instruction_args_for(instruction)
     */
    Ok(())
  }

  fn parse_statement(&mut self) -> Result<(),String> {
    /*
     statement ::= comment | directive | label | instruction
     */

    // A single-line comment is consumed in place.
    if let Some(Token::SingleLineComment(_)) = self.current_token {
      let _ = self.consume_token();
      return Ok(());
    }

    // A directive is identified by a directive marker.
    if let Some(Token::DirectiveMarker) = self.current_token {
      try!(self.parse_directive());
      return Ok(());
    }

    // A label is identified by an id followed by a label marker.
    if let (&Some(Token::Identifier(_)), &Some(Token::LabelMarker)) = (&self.current_token, &self.next_token) {
      try!(self.parse_label());
      return Ok(());
    }

    // Any remaining identifiers should be treated as instructions.
    if let Some(Token::Identifier(_)) = self.current_token {
      return self.parse_instruction();
    }

    Err(format!("Failed to parse statement."))
  }

  fn parse_statement_list(&mut self) -> Result<(),String> {
    /*
     statement_list ::= statement statement_list.
     statement_list ::= .
     */
    while let Some(_) = self.current_token {
      try!(self.parse_statement());
    }
    Ok(())
  }

  // MARK: - Public Methods

  fn parse(&mut self) -> Result<(),String> {
    self.parse_statement_list()
  }

}

// MARK: - Parse Function

pub fn parse<'a, I>(scanner: I) where I: Iterator<Item=Token<'a>> {
  let mut parser = Parser::new(scanner);
  println!("{:?}", parser.parse());
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use super::Parser;

  use assembler::scanner::Scanner;
  use assembler::instructions::Literal;
  use assembler::instructions::{LabelNode, DirectiveNode};

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
    let expected_org = DirectiveNode::new("org", vec![Literal::Numeric(0x100)]);
    assert_eq!(org_parser.parse_directive(), Ok(expected_org));

    // Test a db directive.
    let mut db_parser = Parser::new(Scanner::new(".db \"Hello, World!\", $0\n"));
    let expected_db = DirectiveNode::new("db", vec![Literal::String("Hello, World!"), Literal::Numeric(0)]);
    assert_eq!(db_parser.parse_directive(), Ok(expected_db));

    // Test a fictional argument-free directive.
    let mut test_parser = Parser::new(Scanner::new(".test\n"));
    let expected_test = DirectiveNode::new("test", vec![]);
    assert_eq!(test_parser.parse_directive(), Ok(expected_test));
  }

  #[test]
  fn test_parse_label() {
    // Parse a label.
    let mut parser = Parser::new(Scanner::new("a:\n"));
    let expected_label = LabelNode::new("a");
    assert_eq!(parser.parse_label(), Ok(expected_label));
  }

}
