
use std::fmt;

use assembler::source_file_location::SourceFileLocation;
use assembler::scanner;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token<'a> {
  /// A single-line comment. Value includes ';' but not the newline.
  SingleLineComment(&'a str, SourceFileLocation<'a>),
  /// A period.
  DirectiveMarker(SourceFileLocation<'a>),
  /// An identifier (starts with an _ or a letter, is an underscore or alphanumeric).
  Identifier(&'a str, SourceFileLocation<'a>),
  /// A colon.
  LabelMarker(SourceFileLocation<'a>),
  /// A string literal, contained in double-quotes. Quotes are not included in the value and it is unescaped.
  StringLiteral(&'a str, SourceFileLocation<'a>),
  /// A numeric literal value.
  NumericLiteral(usize, SourceFileLocation<'a>),
  /// List separator token ','.
  Comma(SourceFileLocation<'a>),
  /// Newline, a \n character.
  Newline(SourceFileLocation<'a>),
  /// An including the reason that the tokenization failed.
  Error(scanner::Error<'a>, SourceFileLocation<'a>)
}

impl<'a> Token<'a> {
  /**
   @return A reference to the source file location corresponding to any variant of Token.
   */
  pub fn location(&self) -> &SourceFileLocation<'a> {
    match self {
      &Token::SingleLineComment(_, ref loc)   => loc,
      &Token::DirectiveMarker(ref loc)        => loc,
      &Token::Identifier(_, ref loc)          => loc,
      &Token::LabelMarker(ref loc)            => loc,
      &Token::StringLiteral(_, ref loc)       => loc,
      &Token::NumericLiteral(_, ref loc)      => loc,
      &Token::Comma(ref loc)                  => loc,
      &Token::Newline(ref loc)                => loc,
      &Token::Error(_, ref loc)               => loc
    }
  }
}

impl<'a> fmt::Display for Token<'a> {
  /**
   Formats the receiver for display purposes. This includes the associated value if any
   while omitting the source file location component. Names are lower-case.
   @param f Formatter to write into.
   @return A result representing the formatted receiver or a failure to write into the Formatter.
   */
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Token::SingleLineComment(_, _)         => write!(f, "{}", display_names::SINGLE_LINE_COMMENT),
      &Token::DirectiveMarker(_)              => write!(f, "{}", display_names::DIRECTIVE_MARKER),
      &Token::LabelMarker(_)                  => write!(f, "{}", display_names::LABEL_MARKER),
      &Token::Comma(_)                        => write!(f, "{}", display_names::COMMA),
      &Token::Newline(_)                      => write!(f, "{}", display_names::NEWLINE),
      &Token::Identifier(ref value, _)        => write!(f, "{} ({})", display_names::IDENTIFIER, value),
      &Token::StringLiteral(ref value, _)     => write!(f, "{} (\"{}\")", display_names::STRING_LITERAL, value),
      &Token::NumericLiteral(ref value, _)    => write!(f, "{} ({} / ${:X})", display_names::NUMERIC_LITERAL, value, value),
      &Token::Error(ref reason, _)            => write!(f, "{}", reason)
    }
  }
}

/**
 Un-parameterized display names for tokens.
 */
pub mod display_names {
  pub const SINGLE_LINE_COMMENT: &'static str = "single-line comment";
  pub const DIRECTIVE_MARKER: &'static str    = "directive marker";
  pub const IDENTIFIER: &'static str          = "identifier";
  pub const LABEL_MARKER: &'static str        = "label marker";
  pub const STRING_LITERAL: &'static str      = "string literal";
  pub const NUMERIC_LITERAL: &'static str     = "numeric literal";
  pub const COMMA: &'static str               = "comma";
  pub const NEWLINE: &'static str             = "newline";
  pub const INSTRUCTION_FIELD: &'static str   = "instruction field";
}

#[cfg(test)]
mod tests {
  
  use std::error;
  
  use super::*;

  use assembler::scanner;
  use assembler::source_file_location::SourceFileLocation;

  #[test]
  fn test_display_fmt() {
    let location = SourceFileLocation::new("test_file.s", 1, 1, 1);
    assert_eq!(format!("{}", Token::SingleLineComment("; Hello", location.clone())), "single-line comment");
    assert_eq!(format!("{}", Token::DirectiveMarker(location.clone())), "directive marker");
    assert_eq!(format!("{}", Token::Identifier("test_id", location.clone())), "identifier (test_id)");
    assert_eq!(format!("{}", Token::LabelMarker(location.clone())), "label marker");
    assert_eq!(format!("{}", Token::StringLiteral("Hello", location.clone())), "string literal (\"Hello\")");
    assert_eq!(format!("{}", Token::NumericLiteral(12, location.clone())), "numeric literal (12 / $C)");
    assert_eq!(format!("{}", Token::Comma(location.clone())), "comma");
    assert_eq!(format!("{}", Token::Newline(location.clone())), "newline");

    let error = scanner::Error::InvalidStringLiteral;
    let expected_error_display = format!("{}.", (&error as &error::Error).description());
    assert_eq!(format!("{}", Token::Error(error.clone(), location.clone())), expected_error_display);
  }

}
