//
// Copyright 2016 The c8s Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>.
// All files in the project carrying such notice may not be copied, modified, or 
// distributed except according to those terms.
//

use std::error;
use std::fmt;

use regex::Regex;

use assembler::source_file_location::SourceFileLocation;
use assembler::token::Token;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Error<'a> {
  /// An invalid quoted string literal was encountered at `location`.
  InvalidStringLiteral,
  /// An invalid hexadecimal literal starting with `$` was encountered at `location`.
  InvalidHexadecimalLiteral,
  /// An valid decimal literal was encountered at `location` with `value` not in `0...4095`.
  DecimalLiteralOutOfRange(&'a str),
  /// An invalid decimal literal was encountered at `location`.
  InvalidDecimalLiteral,
  /// An invalid identifier was encountered at `location`.
  InvalidIdentifier,
  /// An expected Register Indirect `[i]` was not matched.
  ExpectedRegisterIndirect,
  /// An invalid character was encountered at `location`.
  InvalidCharacter(char),
}

impl<'a> error::Error for Error<'a> {
  /// Returns a string slice with a general description of a scanner error.
  /// No specific information is contained. To obtain a printable representation,
  /// use the `fmt::Display` attribute.
  fn description(&self) -> &str {
    match self {
      &Error::InvalidStringLiteral        => "Invalid quoted string literal",
      &Error::InvalidHexadecimalLiteral   => "Invalid hexadecimal literal starting with ($)",
      &Error::DecimalLiteralOutOfRange(_) => "Decimal literal not in range 0...4095",
      &Error::InvalidDecimalLiteral       => "Invalid decimal literal encountered",
      &Error::InvalidIdentifier           => "Invalid identifier encountered",
      &Error::ExpectedRegisterIndirect    => "Expected Index Register Indirect ([i]) not found",
      &Error::InvalidCharacter(_)         => "Invalid character encountered",
    }
  }
}

impl<'a> fmt::Display for Error<'a> {
  /// Formats the receiver for display purposes. Incorporates specific information
  /// relating to this particular error instance where applicable.
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Error::DecimalLiteralOutOfRange(literal) =>
        write!(f, "Decimal literal {} not in range (0...4095).", literal),
      &Error::InvalidCharacter(character) =>
        write!(f, "Invalid character '{}'.", character),
      _ =>
        write!(f, "{}.", (self as &error::Error).description())
    }
  }
}

pub struct Scanner<'a> {
  /// The display name of the source file (usually last path component).
  file_name: &'a str,
  /// Input string to process.
  input: &'a str,
  /// Offset from the start of input, in bytes, not in code points.
  position: usize,
  /// Tracks the current line number, starting at 1.
  current_line: usize,
  /// Tracks the offset within the current line, in code points, starting at 1.
  current_line_offset: usize
}

impl<'a> Scanner<'a> {
  
  /**
   Scans forward from the current position and matches all whitespace (if any).
   */
  fn consume_whitespace(&mut self) {
    lazy_static! {
       static ref WHITESPACE: Regex = Regex::new(r"^[^\S\n]+").unwrap();
    }
    match WHITESPACE.find(&self.input[self.position .. ]) {
      Some((0, byte_index_end)) => { let _ = self.advance_by(byte_index_end); }
      _ => {}
    };
  }

  /**
   Matches a single newline character.
   */
  fn consume_newline(&mut self) -> Token<'a> {
    let (value, location) = self.consume_char();
    assert!(value == '\n');
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
      Some((0, byte_index_end)) => {
        // Remove the leading and trailing quotation marks from the string literal.
        let slice_start = self.position + '"'.len_utf8();
        let slice_end = self.position + byte_index_end - '"'.len_utf8();
        let slice = &self.input[slice_start .. slice_end];
        let location = self.advance_by(byte_index_end);
        Token::StringLiteral(slice, location)
      }
      _ => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        let location = SourceFileLocation::new(self.file_name, self.current_line, self.current_line_offset, 1);
        self.position = self.input.len();
        Token::Error(Error::InvalidStringLiteral, location)
      }
    }
  }
  
  /**
   Matches a 4, 8 or 12-bit hexadecimal literal beginning with $.
   @return A token containing the literal value as a usize, or Error in case of invalid formatting.
   */
  fn consume_hex_literal(&mut self) -> Token<'a> {
    lazy_static! {
       static ref HEX_LITERAL: Regex = Regex::new(r#"^[\$\#][0-9a-fA-F]{1,3}"#).unwrap();
    }
    match HEX_LITERAL.find(&self.input[self.position .. ]) {
      Some((0, byte_index_end)) => {
        // Strip off the leading '$' and parse the hexadecimal value.
        let slice_start = self.position + '$'.len_utf8();
        let slice_end = self.position + byte_index_end;
        let slice = &self.input[slice_start .. slice_end];
        let location = self.advance_by(byte_index_end);
        Token::NumericLiteral(usize::from_str_radix(slice, 16).ok().unwrap(), location)
      }
      _ => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        let location = SourceFileLocation::new(self.file_name, self.current_line, self.current_line_offset, 1);
        self.position = self.input.len();
        Token::Error(Error::InvalidHexadecimalLiteral, location)
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
      Some((0, byte_index_end)) => {
        // Strip off the leading '$' and parse the hexadecimal value.
        let slice_start = self.position;
        let slice_end = self.position + byte_index_end;
        let slice = &self.input[slice_start .. slice_end];
        
        // Match on a decimal literal in the range 0...4095.
        match usize::from_str_radix(slice, 10).ok() {
          Some(value) if value <= 4095 => {
            let location = self.advance_by(byte_index_end);
            Token::NumericLiteral(value, location)
          }
          _ => {
            // Push to the end of the input to indicate parse failure.
            let location = self.advance_by(byte_index_end);
            self.position = self.input.len();
            Token::Error(Error::DecimalLiteralOutOfRange(slice), location)
          }
        }
      }
      _ => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        let location = SourceFileLocation::new(self.file_name, self.current_line, self.current_line_offset, 1);
        self.position = self.input.len();
        Token::Error(Error::InvalidDecimalLiteral, location)
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
      Some((0, byte_index_end)) => {
        // Strip off the leading '$' and parse the hexadecimal value.
        let slice = &self.input[self.position .. self.position + byte_index_end];
        let location = self.advance_by(byte_index_end);
        Token::Identifier(slice, location)
      }
      _ => {
        // Advance to the end of the input to terminate the parse and indicate failure.
        let location = SourceFileLocation::new(self.file_name, self.current_line, self.current_line_offset, 1);
        self.position = self.input.len();
        Token::Error(Error::InvalidIdentifier, location)
      }
    }
  }

  /**
   Consumes an Index Register Indirect identifier, [i].
   @return A token containing the identifier, or Error in case of invalid formatting.
   */
  fn consume_index_register_indirect(&mut self) -> Token<'a> {
    let index_register_indirect = "[i]";
    let index_register_indirect_upper = "[I]";

    // Offset the input to the current position.
    let slice = &self.input[self.position .. ];

    // The Index Register Indirect identifier, [i] is consumed as an identifier.
    if slice.starts_with(index_register_indirect) || slice.starts_with(index_register_indirect_upper) {
      let length = index_register_indirect.len();
      let slice = &self.input[self.position .. self.position + length];
      let location = self.advance_by(length);
      return Token::Identifier(slice, location);
    }
    
    let location = SourceFileLocation::new(self.file_name, self.current_line, self.current_line_offset, 1);
    self.position = self.input.len();
    Token::Error(Error::ExpectedRegisterIndirect, location)
  }
  
  /**
   Consumes a single-line comment, beginning with a ';' and ending with '\n'.
   This method will panic if the first character to be processed is not a ';'.
   @return A token containing the entirety of the comment, including the ';' but not the newline.
   */
  fn consume_single_line_comment(&mut self) -> Token<'a> {
    assert!(self.char_at(0) == ';');
    
    // Determine how many bytes to consume, including the ';' but not the \n.
    let mut bytes = 0;
    for c in self.input[self.position .. ].chars() {
      if c == '\n' {
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
  fn consume_char(&mut self) -> (char, SourceFileLocation<'a>) {
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
   Advances the position counter, line and line offset based on the next n bytes.
   @return The source file location corresponding to the region advanced over.
   */
  fn advance_by(&mut self, bytes: usize) -> SourceFileLocation<'a> {
    let range = &self.input[self.position .. (self.position + bytes)];

    // Count the number of code points in the byte range and sanity-check.
    let total_byte_count = range.chars().fold(0, |acc, x| acc + x.len_utf8());
    assert!(total_byte_count == bytes, "Byte length of range doesn't line up with code points.");

    // Track the number of newlines, characters on the first line and build the new last-line offset.
    let mut number_of_newlines = 0;
    let mut characters_on_first_line = 0;
    let mut last_line_offset = self.current_line_offset;

    for character in range.chars() {
      last_line_offset += 1;

      // Count characters on first line up to and including the terminator.
      if number_of_newlines == 0 { characters_on_first_line += 1; }

      // On newline, bump the newline counter and reset the last-line offset.
      if character == '\n' {
        number_of_newlines += 1;
        last_line_offset = 1;
      }
    }

    // The resultant location spans all characters up to and including any null-terminator on the first line in the range.
    let location = SourceFileLocation::new(self.file_name, self.current_line, self.current_line_offset, characters_on_first_line);
    self.position += bytes;
    self.current_line += number_of_newlines;
    self.current_line_offset = last_line_offset;
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
      '#' => { Some(self.consume_hex_literal()) }
      '[' => { Some(self.consume_index_register_indirect()) }
      
      '0' ... '9' => {
        Some(self.consume_decimal_literal()) 
      }
      
      '_' | 'a' ... 'z' | 'A' ... 'Z' => {
        Some(self.consume_identifier()) 
      }

      '\n' => {
        Some(self.consume_newline())
      }
       
      c @ _  => { 
        // Advance to end of input on invalid token.
        let location = SourceFileLocation::new(self.file_name, self.current_line, self.current_line_offset, 1);
        self.position = self.input.len();
        Some(Token::Error(Error::InvalidCharacter(c), location))
      }
    }
  }
  
  /**
   @return A new scanner over the provided input.
   */
  pub fn new(file_name: &'a str, input: &'a str) -> Self {
    Scanner {
      file_name: file_name,
      input: input,
      position: 0,
      current_line: 1,
      current_line_offset: 1
    }
  }
  
}

impl<'a> Iterator for Scanner<'a> {
  type Item = Token<'a>;
  fn next(&mut self) -> Option<Self::Item> {
    self.next_token()
  }
}

#[cfg(test)]
mod tests {
  
  use assembler::token::Token;
  use assembler::source_file_location::SourceFileLocation;

  use super::*;

  #[test]
  fn test_is_at_end_empty_string() {
    let mut scanner = Scanner::new("-", "");
    assert_eq!(scanner.is_at_end(), true);
    assert_eq!(scanner.next(), None);
  }
  
  #[test]
  fn test_is_at_end_non_empty_string() {
    let scanner = Scanner::new("-", "NON_EMPTY_STRING");
    assert_eq!(scanner.is_at_end(), false);
  }
  
  #[test]
  fn test_next_whitespace_only() {
    let mut scanner = Scanner::new("-", "    \t");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
  // MARK: - Single-Character Tokens
  
  #[test]
  fn test_directive_marker() {
    let mut scanner = Scanner::new("-", ".");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::DirectiveMarker(SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_label_marker() {
    let mut scanner = Scanner::new("-", ":");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::LabelMarker(SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_comma() {
    let mut scanner = Scanner::new("-", ",");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Comma(SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  // MARK: - Non-Coding

  #[test]
  fn test_newline() {
    let mut scanner = Scanner::new("-", "\n\n \n");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new("-", 2, 1, 1))));
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new("-", 3, 2, 1))));
    assert_eq!(scanner.is_at_end(), true);
  }

  #[test]
  fn test_comment() {
    let mut scanner = Scanner::new("-", ";");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::SingleLineComment(";", SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new("-", ";\n");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::SingleLineComment(";", SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new("-", 1, 2, 1))));
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new("-", "; Single-Line Comment\n");
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::SingleLineComment("; Single-Line Comment", SourceFileLocation::new("-", 1, 1, 21))));
    assert_eq!(scanner.is_at_end(), false);
    assert_eq!(scanner.next(), Some(Token::Newline(SourceFileLocation::new("-", 1, 22, 1))));
    assert_eq!(scanner.is_at_end(), true);
  }
  
  // MARK: - Value Types
  
  #[test]
  fn test_identifier() {
    let mut scanner = Scanner::new("-", "_ _a a _A A _0 _aA _zZ9");
    assert_eq!(scanner.next(), Some(Token::Identifier("_", SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_a", SourceFileLocation::new("-", 1, 3, 2))));
    assert_eq!(scanner.next(), Some(Token::Identifier("a", SourceFileLocation::new("-", 1, 6, 1))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_A", SourceFileLocation::new("-", 1, 8, 2))));
    assert_eq!(scanner.next(), Some(Token::Identifier("A", SourceFileLocation::new("-", 1, 11, 1))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_0", SourceFileLocation::new("-", 1, 13, 2))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_aA", SourceFileLocation::new("-", 1, 16, 3))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_zZ9", SourceFileLocation::new("-", 1, 20, 4))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new("-", "_&");
    assert_eq!(scanner.next(), Some(Token::Identifier("_", SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.next(), Some(Token::Error(Error::InvalidCharacter('&'), SourceFileLocation::new("-", 1, 2, 1))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
    
    scanner = Scanner::new("-", "_$0");
    assert_eq!(scanner.next(), Some(Token::Identifier("_", SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0, SourceFileLocation::new("-", 1, 2, 2))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }

  #[test]
  fn test_index_register_indirect() {
    let mut scanner = Scanner::new("-", "[i] [a]");
    assert_eq!(scanner.next(), Some(Token::Identifier("[i]", SourceFileLocation::new("-", 1, 1, 3))));
    assert_eq!(scanner.next(), Some(Token::Error(Error::ExpectedRegisterIndirect, SourceFileLocation::new("-", 1, 5, 1))));
  }
  
  #[test]
  fn test_numeric_literal_hex() {
    let mut scanner = Scanner::new("-", "$0 $00 $000 $1 $F $FF $1FF $1_0 $");
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x0, SourceFileLocation::new("-", 1, 1, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x00, SourceFileLocation::new("-", 1, 4, 3))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x000, SourceFileLocation::new("-", 1, 8, 4))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1, SourceFileLocation::new("-", 1, 13, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0xF, SourceFileLocation::new("-", 1, 16, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0xFF, SourceFileLocation::new("-", 1, 19, 3))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1FF, SourceFileLocation::new("-", 1, 23, 4))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1, SourceFileLocation::new("-", 1, 28, 2))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_0", SourceFileLocation::new("-", 1, 30, 2))));
    assert_eq!(scanner.next(), Some(Token::Error(Error::InvalidHexadecimalLiteral, SourceFileLocation::new("-", 1, 33, 1))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }

  #[test]
  fn test_numeric_literal_alternate_leading_indicator() {
    let mut scanner = Scanner::new("-", "#0 #00 #000 #1 #F #FF #1FF #1_0 #");
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x0, SourceFileLocation::new("-", 1, 1, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x00, SourceFileLocation::new("-", 1, 4, 3))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x000, SourceFileLocation::new("-", 1, 8, 4))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1, SourceFileLocation::new("-", 1, 13, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0xF, SourceFileLocation::new("-", 1, 16, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0xFF, SourceFileLocation::new("-", 1, 19, 3))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1FF, SourceFileLocation::new("-", 1, 23, 4))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0x1, SourceFileLocation::new("-", 1, 28, 2))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_0", SourceFileLocation::new("-", 1, 30, 2))));
    assert_eq!(scanner.next(), Some(Token::Error(Error::InvalidHexadecimalLiteral, SourceFileLocation::new("-", 1, 33, 1))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_numeric_literal_dec() {
    let mut scanner = Scanner::new("-", "0 00 000 1 15 255 4095 1_0 4096");
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0, SourceFileLocation::new("-", 1, 1, 1))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0, SourceFileLocation::new("-", 1, 3, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(0, SourceFileLocation::new("-", 1, 6, 3))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(1, SourceFileLocation::new("-", 1, 10, 1))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(15, SourceFileLocation::new("-", 1, 12, 2))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(255, SourceFileLocation::new("-", 1, 15, 3))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(4095, SourceFileLocation::new("-", 1, 19, 4))));
    assert_eq!(scanner.next(), Some(Token::NumericLiteral(1, SourceFileLocation::new("-", 1, 24, 1))));
    assert_eq!(scanner.next(), Some(Token::Identifier("_0", SourceFileLocation::new("-", 1, 25, 2))));
    assert_eq!(scanner.next(), Some(Token::Error(Error::DecimalLiteralOutOfRange("4096"), SourceFileLocation::new("-", 1, 28, 4))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
  #[test]
  fn test_string_literal() {
    let mut scanner = Scanner::new("-", "\"\" \"a\" \"😊😞\" \"123\" \"end-of-line");
    assert_eq!(scanner.next(), Some(Token::StringLiteral("", SourceFileLocation::new("-", 1, 1, 2))));
    assert_eq!(scanner.next(), Some(Token::StringLiteral("a", SourceFileLocation::new("-", 1, 4, 3))));
    assert_eq!(scanner.next(), Some(Token::StringLiteral("😊😞", SourceFileLocation::new("-", 1, 8, 4))));
    assert_eq!(scanner.next(), Some(Token::StringLiteral("123", SourceFileLocation::new("-", 1, 13, 5))));
    assert_eq!(scanner.next(), Some(Token::Error(Error::InvalidStringLiteral, SourceFileLocation::new("-", 1, 19, 1))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }

  #[test]
  fn test_string_literal_multiline() {
    let mut scanner = Scanner::new("-", "\"Hello 😊\n🐳 World\"");
    assert_eq!(scanner.next(), Some(Token::StringLiteral("Hello 😊\n🐳 World", SourceFileLocation::new("-", 1, 1, 9))));
    assert_eq!(scanner.next(), None);
    assert_eq!(scanner.is_at_end(), true);
  }
  
}
