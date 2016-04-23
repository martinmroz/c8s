
use regex::Regex;

#[derive(PartialEq, Debug, Clone)]
pub struct SourceFileLocation {
  /// The offset from the start of input, in bytes.
  location: usize,
  /// The length of the region, in bytes.
  length: usize
}

impl SourceFileLocation {
  fn new(location: usize, length: usize) -> SourceFileLocation {
    SourceFileLocation {
      location: location,
      length: length
    }
  }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
  /// A single-line comment. Value includes ';' but not the newline.
  SingleLineComment(&'a str, SourceFileLocation),
  /// A period.
  DirectiveMarker(SourceFileLocation),
  /// An identifier (starts with an _ or a letter, is an underscore or alphanumeric).
  Identifier(&'a str),
  /// A colon.
  LabelMarker(SourceFileLocation),
  /// A string literal, contained in double-quotes. Quotes are not included in the value.
  StringLiteral(&'a str),
  /// A numeric literal value.
  NumericLiteral(usize),
  /// List separator token ','.
  Comma(SourceFileLocation),
  /// Newline, a \n character.
  Newline(SourceFileLocation),
  /// An including the reason that the tokenization failed.
  Error(String)
}

pub struct Scanner<'a> {
  /// Input string to process.
  input: &'a str,
  /// Offset from the start of input, in bytes, not in code points.
  position: usize
}

impl<'a> Scanner<'a> {
  
  /**
   Scans forward from the current position and matches all whitespace (if any).
   */
  fn consume_whitespace(&mut self) {
    lazy_static! {
       static ref WHITESPACE: Regex = Regex::new(r"^[^\S\n\r]+").unwrap();
    }
    match WHITESPACE.find(&self.input[self.position .. ]) {
      Some((_, byte_index_end)) => { self.advance_by(byte_index_end); }
      None => {}
    };
  }

  /**
   Matches a run of newline characters.
   */
  fn consume_newline(&mut self) -> Token<'a> {
    assert!(self.char_at(0) == '\r' || self.char_at(0) == '\n');
    
    // Determine how many bytes to consume in the run.
    let mut bytes = 0;
    for c in self.input[self.position .. ].chars() {
      if c == '\r' || c == '\n' {
        bytes = bytes + c.len_utf8();
      } else {
        break;
      }
    }
    
    let location = self.advance_by(bytes);
    Token::Newline(location)
  }
  
  /**
   Matches a quoted string literal, including embedded backslash-escaped quote characters.
   @return A token containing the body of the string literal (still escaped), or Error.
   */
  fn consume_string_literal(&mut self) -> Token<'a> {
    lazy_static! {
       static ref STRING_LITERAL: Regex = Regex::new(r#"^"(?:[^"\\]|\\.)*""#).unwrap();
    }
    match STRING_LITERAL.find(&self.input[self.position .. ]) {
      Some((_, byte_index_end)) => {
        // Remove the leading and trailing quotation marks from the string literal.
        // TODO: Un-escape the parsed string literal.
        // TODO: Do not match un-terminated string literals.
        let slice_start = self.position + '"'.len_utf8();
        let slice_end = self.position + byte_index_end - '"'.len_utf8();
        let slice = &self.input[slice_start .. slice_end];
        self.advance_by(byte_index_end);
        Token::StringLiteral(slice)
      }
      None => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        self.position = self.input.len();
        Token::Error("Invalid quoted string literal.".to_string())
      }
    }
  }
  
  /**
   Matches a 4, 8 or 12-bit hexadecimal literal beginning with $.
   @return A token containing the literal value as a usize, or Error in case of invalid formatting.
   */
  fn consume_hex_literal(&mut self) -> Token<'a> {
    lazy_static! {
       static ref HEX_LITERAL: Regex = Regex::new(r#"^\$[0-9a-fA-F]{1,3}"#).unwrap();
    }
    match HEX_LITERAL.find(&self.input[self.position .. ]) {
      Some((_, byte_index_end)) => {
        // Strip off the leading '$' and parse the hexadecimal value.
        let slice_start = self.position + '$'.len_utf8();
        let slice_end = self.position + byte_index_end;
        let slice = &self.input[slice_start .. slice_end];
        self.advance_by(byte_index_end);
        Token::NumericLiteral(usize::from_str_radix(slice, 16).ok().unwrap())
      }
      None => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        self.position = self.input.len();
        Token::Error("Invalid hexadecimal literal starting with ($).".to_string())
      }
    }
  }
  
  /**
   Matches a decimal literal from 0-4095.
   @return A token containing the literal value as a usize, or Error in case of invalid formatting.
   */
  fn consume_decimal_literal(&mut self) -> Token<'a> {
    lazy_static! {
       static ref DECIMAL_LITERAL: Regex = Regex::new(r#"^[0-9]{1,}"#).unwrap();
    }
    match DECIMAL_LITERAL.find(&self.input[self.position .. ]) {
      Some((_, byte_index_end)) => {
        // Strip off the leading '$' and parse the hexadecimal value.
        let slice_start = self.position;
        let slice_end = self.position + byte_index_end;
        let slice = &self.input[slice_start .. slice_end];
        
        // Match on a decimal literal in the range 0...4095
        match usize::from_str_radix(slice, 10).ok() {
          Some(value) if value <= 4095 => {
            self.advance_by(byte_index_end);
            Token::NumericLiteral(value)
          }
          _ => {
            // Push to the end of the input to indicate parse failure.
            self.position = self.input.len();
            Token::Error(format!("Decimal literal {} out of range (0...4095).", slice))
          }
        }
      }
      None => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        self.position = self.input.len();
        Token::Error("Invalid hexadecimal literal starting with ($).".to_string())
      }
    }
  }
  
  /**
   Matches an identifier beginning with _ or a letter, then a run of _, numbers or letters.
   @return A token containing the name of the identifier, or Error in case of invalid formatting.
   */
  fn consume_identifier(&mut self) -> Token<'a> {
    lazy_static! {
       static ref IDENTIFIER: Regex = Regex::new(r#"^[_a-zA-Z][_a-zA-Z0-9]{0,}"#).unwrap();
    }
    match IDENTIFIER.find(&self.input[self.position .. ]) {
      Some((_, byte_index_end)) => {
        // Strip off the leading '$' and parse the hexadecimal value.
        let slice = &self.input[self.position .. self.position + byte_index_end];
        self.advance_by(byte_index_end);
        Token::Identifier(slice)
      }
      None => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        self.position = self.input.len();
        Token::Error("Invalid identifier.".to_string())
      }
    }
  }

  /**
   Consumes an Index Register Indirect identifier, [i].
   @return A token containing the identifier, or Error in case of invalid formatting.
   */
  fn consume_index_register_indirect(&mut self) -> Token<'a> {
    let mut char_iter = self.input[self.position .. ].chars();

    // The Index Register Indirect identifier, [i] is consumed as an identifier.
    if let (Some('['), Some('i'), Some(']')) = (char_iter.next(), char_iter.next(), char_iter.next()) {
      let length = "[i]".len();
      let slice = &self.input[self.position .. self.position + length];
      self.advance_by(length);
      return Token::Identifier(slice);
    }
    
    Token::Error("Expected Index Register Indirect ([i]) not found.".to_string())
  }
  
  /**
   Consumes a single-line comment, beginning with a ';' and ending with '\r' or '\n'.
   This method will panic if the first character to be processed is not a ';'.
   @return A token containing the entirety of the comment, including the ';' but not the newline.
   */
  fn consume_single_line_comment(&mut self) -> Token<'a> {
    assert!(self.char_at(0) == ';');
    
    // Determine how many bytes to consume, including the ';' but not the \n.
    let mut bytes = 0;
    for c in self.input[self.position .. ].chars() {
      if c == '\r' || c == '\n' {
        break;
      } else { 
        bytes = bytes + c.len_utf8();
      }
    }
    
    let slice = &self.input[self.position .. self.position + bytes];
    let location = self.advance_by(bytes);
    Token::SingleLineComment(slice, location)
  }
  
  /**
   @return A token representing a directive marker.
   */
  fn consume_directive_marker(&mut self) -> Token<'a> {
    let (value, location) = self.consume_char();
    assert!(value == '.');
    Token::DirectiveMarker(location)
  }
  
  /**
   @return A token representing a label marker.
   */
  fn consume_label_marker(&mut self) -> Token<'a> {
    let (value, location) = self.consume_char();
    assert!(value == ':');
    Token::LabelMarker(location)
  }
  
  /**
   @return A token representing a comma.
   */
  fn consume_comma(&mut self) -> Token<'a> {
    let (value, location) = self.consume_char();
    assert!(value == ',');
    Token::Comma(location)
  }
  
  /**
   Consumes the next unicode code point.
   @return The consumed character and its location in the source file.
   */
  fn consume_char(&mut self) -> (char, SourceFileLocation) {
    let value = self.char_at(0);
    (value, self.advance_by(value.len_utf8()))
  }
  
  /**
   @return The unicode character at the specified byte offset.
   */
  fn char_at(&self, offset: usize) -> char {
    self.input[self.position + offset .. ].chars().next().unwrap()
  }
  
  /**
   Advances the position counter by the specified number of bytes.
   @return The source file location corresponding to the region advanced over.
   */
  fn advance_by(&mut self, bytes: usize) -> SourceFileLocation {
    let location = SourceFileLocation::new(self.position, bytes);
    self.position += bytes;
    location
  }

  /**
   @return true if the scanner has reached the end of the input.
   */
  pub fn is_at_end(&self) -> bool {
    self.position >= self.input.len()
  }
  
  /**
   @return The next token, or None if the scanner has reached the end of the input.
   */
  pub fn next_token(&mut self) -> Option<Token<'a>> {
    self.consume_whitespace();
    if self.is_at_end() {
      return None;
    }
    
    match self.char_at(0) {
      ';' => { Some(self.consume_single_line_comment()) }
      '.' => { Some(self.consume_directive_marker()) }
      ':' => { Some(self.consume_label_marker()) }
      ',' => { Some(self.consume_comma()) }
      '"' => { Some(self.consume_string_literal()) }
      '$' => { Some(self.consume_hex_literal()) }
      '[' => { Some(self.consume_index_register_indirect()) }
      
      '0' ... '9' => {
        Some(self.consume_decimal_literal()) 
      }
      
      '_' | 'a' ... 'z' | 'A' ... 'Z' => {
        Some(self.consume_identifier()) 
      }

      '\r' | '\n' => {
        Some(self.consume_newline())
      }
       
      c @ _  => { 
        // Advance to end of input on invalid token.
        self.position = self.input.len();
        Some(Token::Error(format!("Invalid character '{}'", c)))
      }
    }
  }
  
  /**
   @return A new scanner over the provided input.
   */
  pub fn new(input: &'a str) -> Scanner<'a> {
    Scanner {
      input: input,
      position: 0
    }
  }
  
}

impl<'a> Iterator for Scanner<'a> {
  type Item = Token<'a>;
  fn next(&mut self) -> Option<Token<'a>> {
    self.next_token()
  }
}

#[cfg(test)]
mod tests {
  
  use super::*;
  
  #[test]
  fn test_is_at_end_empty_string() {
    let mut scanner = Scanner::new("");
    assert_eq!(scanner.is_at_end(), true);
    assert_eq!(scanner.next(), None);
  }
  
  #[test]
  fn test_is_at_end_non_empty_string() {
    let scanner = Scanner::new("NON_EMPTY_STRING");
    assert_eq!(scanner.is_at_end(), false);
  }
  
  #[test]
  fn test_next_whitespace_only() {
    let mut scanner = Scanner::new("    \t");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
  // MARK: - Single-Character Tokens
  
  #[test]
  fn test_directive_marker() {
    let mut scanner = Scanner::new(".");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::DirectiveMarker(SourceFileLocation::new(0,1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_label_marker() {
    let mut scanner = Scanner::new(":");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::LabelMarker(SourceFileLocation::new(0,1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_comma() {
    let mut scanner = Scanner::new(",");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Comma(SourceFileLocation::new(0,1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  // MARK: - Non-Coding
  
  #[test]
  fn test_newline() {
    let mut scanner = Scanner::new("\r\n\r\n\n \n");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new(0,5))));
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new(6,1))));
    assert_eq!(scanner.is_at_end(), true);
  }

  #[test]
  fn test_comment() {
    let mut scanner = Scanner::new(";");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::SingleLineComment(";", SourceFileLocation::new(0,1))));
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new(";\n");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::SingleLineComment(";", SourceFileLocation::new(0,1))));
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new(1,1))));
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new("; Single-Line Comment\n");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::SingleLineComment("; Single-Line Comment", SourceFileLocation::new(0,21))));
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new(21,1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  // MARK: - Value Types
  
  #[test]
  fn test_identifier() {
    let mut scanner = Scanner::new("_ _a a _A A _0 _aA _zZ9");
    assert_eq!(scanner.next(), Some(Token::Identifier("_")));
    assert_eq!(scanner.next(), Some(Token::Identifier("_a")));
    assert_eq!(scanner.next(), Some(Token::Identifier("a")));
    assert_eq!(scanner.next(), Some(Token::Identifier("_A")));
    assert_eq!(scanner.next(), Some(Token::Identifier("A")));
    assert_eq!(scanner.next(), Some(Token::Identifier("_0")));
    assert_eq!(scanner.next(), Some(Token::Identifier("_aA")));
    assert_eq!(scanner.next(), Some(Token::Identifier("_zZ9")));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new("_&");
    assert_eq!(scanner.next(), Some(Token::Identifier("_")));
    assert_eq!(scanner.next(), Some(Token::Error("Invalid character '&'".to_string())));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new("_$0");
    assert_eq!(scanner.next(), Some(Token::Identifier("_")));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0)));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }

  #[test]
  fn test_index_register_indirect() {
    let mut scanner = Scanner::new("[i] [a]");
    assert_eq!(scanner.next(), Some(Token::Identifier("[i]")));
    assert_eq!(scanner.next(), Some(Token::Error("Expected Index Register Indirect ([i]) not found.".to_string())));
  }
  
  #[test]
  fn test_numeric_literal_hex() {
    let mut scanner = Scanner::new("$0 $00 $000 $1 $F $FF $1FF $1_0 $");
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x0)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x00)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x000)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0xF)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0xFF)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1FF)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1)));
    assert_eq!(scanner.next(), Some(Token::Identifier("_0")));
    assert_eq!(scanner.next(), Some(Token::Error("Invalid hexadecimal literal starting with ($).".to_string())));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_numeric_literal_dec() {
    let mut scanner = Scanner::new("0 00 000 1 15 255 4095 1_0 4096");
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(1)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(15)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(255)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(4095)));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(1)));
    assert_eq!(scanner.next(), Some(Token::Identifier("_0")));
    assert_eq!(scanner.next(), Some(Token::Error(format!("Decimal literal 4096 out of range (0...4095)."))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_string_literal() {
    let mut scanner = Scanner::new("\"\" \"a\" \"123\" \"end-of-line");
    assert_eq!(scanner.next(), Some(Token::StringLiteral("")));
    assert_eq!(scanner.next(), Some(Token::StringLiteral("a")));
    assert_eq!(scanner.next(), Some(Token::StringLiteral("123")));
    assert_eq!(scanner.next(), Some(Token::Error("Invalid quoted string literal.".to_string())));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
}
