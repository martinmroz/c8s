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

use twelve_bit::u12::*;

use assembler::parser::Literal;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Error {
  /// An invalid number of parameters (`$1`) was specified for `directive` needing `$2` (or more (`$3`)).
  IncorrectNumberOfParameters(&'static str, usize, usize, bool),
  /// Directive requires one numeric literal parameter.
  DirectiveRequiresOneNumericLiteral(&'static str),
  /// All numeric parameters must be one-byte literals.
  AllNumericParametersMustBeByteLiterals(&'static str, usize),
  /// The provided directive was unrecognized.
  UnrecognizedDirective(String)
}

impl error::Error for Error {
  /// Returns a string slice with a general description of a directive error.
  /// No specific information is contained. To obtain a printable representation,
  /// use the `fmt::Display` attribute.
  fn description(&self) -> &str {
    match self {
      &Error::IncorrectNumberOfParameters(_,_,_,_)        => "Incorrect number of parameters for directive",
      &Error::DirectiveRequiresOneNumericLiteral(_)       => "Directive requires one numeric literal",
      &Error::AllNumericParametersMustBeByteLiterals(_,_) => "All numeric parameters must by one-byte literals",
      &Error::UnrecognizedDirective(_)                    => "Unrecognized directive",
    }
  }
}

impl fmt::Display for Error {
  /// Formats the receiver for display purposes. Incorporates specific information
  /// relating to this particular error instance where applicable.
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Error::IncorrectNumberOfParameters(directive, count, from, false) =>
        write!(f, "Incorrect number of parameters ({}) for directive .{}, expecting {}.", count, directive, from),
      &Error::IncorrectNumberOfParameters(directive, count, from, true) =>
        write!(f, "Incorrect number of parameters ({}) for directive .{}, expecting {} or more.", count, directive, from),
      &Error::DirectiveRequiresOneNumericLiteral(directive) =>
        write!(f, "Directive .{} requires 1 numeric literal in the range $000-$FFF.", directive),
      &Error::AllNumericParametersMustBeByteLiterals(directive, value_over) =>
        write!(f, "All numeric parameters to .{} must be 1-byte literals (${:X} > $FF).", directive, value_over),
      &Error::UnrecognizedDirective(ref identifier) =>
        write!(f, "Unrecognized directive '.{}'.", identifier),
    }
  }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Directive {
  /// Directive to set the emit origin to the new address.
  Org(U12),
  /// Directive to emit the specified bytes.
  Db(Vec<u8>)
}

impl fmt::Display for Directive {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Directive::Org(location) => write!(f, ".{} ${:3X}",     names::ORG, usize::from(location)),
      &Directive::Db(ref bytes) => write!(f, ".{} [{} bytes]", names::DB,  bytes.len())
    }
  }
}

mod names {
  /// The name of the 'origin' directive (.org)
  pub const ORG: &'static str = "org";
  /// The name of the 'define-bytes' directive (.db)
  pub const DB : &'static str = "db" ;
}

impl<'a> Directive {

  /** 
   Validate the semantics of a directive and yield the Emittable representation.
   @param identifier The identifier for a directive.
   @param arguments The literal arguments specified for the directive.
   @return Directive in the event the arguments are valid for the directive, or a failure reason.
   */
  pub fn from_identifier_and_parameters(input_identifier: &'a str, arguments: &Vec<Literal>) -> Result<Self,Error> {
    let identifer_string_lowercase = String::from(input_identifier).to_lowercase();
    let identifier = identifer_string_lowercase.as_str();

    /*
     The 'org' directive is used to set the current output origin address. The directive requires
     a single numeric literal in the range $000-$FFF.
     */
    if identifier == names::ORG {
      if arguments.len() != 1 {
        return Err(Error::IncorrectNumberOfParameters(names::ORG, arguments.len(), 1, false));
      }

      return match arguments[0] {
        Literal::Numeric(a) if a <= 0xFFF => {
          Ok(Directive::Org(a.unchecked_into()))
        }
        _ => {
          Err(Error::DirectiveRequiresOneNumericLiteral(names::ORG))
        }
      }
    }

    /*
     The 'db' directive is used to emit a series of bytes at the current location. 
     A list of 1 or more literals is required, either numeric or string. 
     A string literal is emitted without a null-terminator.
     */
    if identifier == names::DB {
      if arguments.len() == 0 {
        return Err(Error::IncorrectNumberOfParameters(names::DB, 0, 1, true));
      }

      let mut bytes = Vec::new();

      // Convert all valid parameters into their byte literal values.
      for argument in arguments {
        match argument {
          &Literal::Numeric(value) => {
            if value > 0xFF {
              return Err(Error::AllNumericParametersMustBeByteLiterals(names::DB, value));
            } else {
              bytes.push((value & 0x00FF) as u8);
            }
          }
          &Literal::String(string) => {
            bytes.extend_from_slice(string.as_bytes());
          }
        }
      }

      return Ok(Directive::Db(bytes));
    }

    Err(Error::UnrecognizedDirective(String::from(input_identifier)))
  }

  /**
   @return The size of the directive in the output stream in bytes.
   */
  pub fn size(&self) -> usize {
    match *self {
      Directive::Org(_)       => 0,
      Directive::Db(ref data) => data.len()
    }
  }

  /**
   Consuming conversion from a directive element into the byte representation in the output stream.
   @return A Vec of bytes to be written into the output stream for the directive.
   */
  pub fn into_vec(self) -> Vec<u8> {
    match self {
      Directive::Org(_)       => Vec::new(),
      Directive::Db(data)     => data
    }
  }

}

// MARK: - Tests

#[cfg(test)]
mod tests {

  use twelve_bit::u12;

  use assembler::parser::Literal;

  use super::names;
  use super::*;

  #[test]
  fn test_error_display() {
    assert_eq!(format!("{}", Error::IncorrectNumberOfParameters(names::ORG, 0, 1, false)),
      "Incorrect number of parameters (0) for directive .org, expecting 1.");
    assert_eq!(format!("{}", Error::IncorrectNumberOfParameters(names::ORG, 2, 1, false)),
      "Incorrect number of parameters (2) for directive .org, expecting 1.");
    assert_eq!(format!("{}", Error::IncorrectNumberOfParameters(names::ORG, 0, 1, true)),
      "Incorrect number of parameters (0) for directive .org, expecting 1 or more.");
    assert_eq!(format!("{}", Error::DirectiveRequiresOneNumericLiteral(names::DB)), 
      "Directive .db requires 1 numeric literal in the range $000-$FFF.");
    assert_eq!(format!("{}", Error::AllNumericParametersMustBeByteLiterals(names::ORG, 0x100)),
      "All numeric parameters to .org must be 1-byte literals ($100 > $FF).");
    assert_eq!(format!("{}", Error::UnrecognizedDirective(String::from("dw"))), 
      "Unrecognized directive '.dw'.");
  }

  #[test]
  fn test_validate_directive_semantics_for_org() {
    let mut params = vec![];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err(Error::IncorrectNumberOfParameters(names::ORG, 0, 1, false)));
    params = vec![Literal::Numeric(1), Literal::Numeric(3)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err(Error::IncorrectNumberOfParameters(names::ORG, 2, 1, false)));
    params = vec![Literal::Numeric(0x1000)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err(Error::DirectiveRequiresOneNumericLiteral(names::ORG)));
    params = vec![Literal::String("TEST_STRING")];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err(Error::DirectiveRequiresOneNumericLiteral(names::ORG)));
    params = vec![Literal::Numeric(0xFFF)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Ok(Directive::Org(u12::MAX)));
    params = vec![Literal::Numeric(0x000)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Ok(Directive::Org(u12::MIN)));
  }

  #[test]
  fn test_validate_directive_semantics_for_db() {
    let mut params = vec![];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Err(Error::IncorrectNumberOfParameters(names::DB, 0, 1, true)));
    params = vec![Literal::Numeric(0x100)];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Err(Error::AllNumericParametersMustBeByteLiterals(names::DB, 0x100)));
    params = vec![Literal::String("TEST_STRING")];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Ok(Directive::Db("TEST_STRING".as_bytes().to_vec())));
    params = vec![Literal::Numeric(0xFF)];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Ok(Directive::Db(vec![0xFF])));
    params = vec![Literal::Numeric(0x00)];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Ok(Directive::Db(vec![0x00])));
    params = vec![Literal::String("TEST_STRING"), Literal::Numeric(0x00)];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Ok(Directive::Db("TEST_STRING\0".as_bytes().to_vec())));
  }

  #[test]
  fn test_validate_directive_semantics_for_invalid_directive() {
    let mut params = vec![];
    assert_eq!(Directive::from_identifier_and_parameters("dw", &params), Err(Error::UnrecognizedDirective(String::from("dw"))));
    params = vec![Literal::Numeric(0x100)];
    assert_eq!(Directive::from_identifier_and_parameters("dw", &params), Err(Error::UnrecognizedDirective(String::from("dw"))));
  }

  #[test]
  fn test_size_of_directive_for_org() {
    let org_directive = Directive::from_identifier_and_parameters("org", &vec![Literal::Numeric(0xFFF)]).ok().unwrap();
    assert_eq!(org_directive.size(), 0);
  }

  #[test]
  fn test_size_of_directive_for_db() {
    let test_cases = [
      ( 1, Directive::from_identifier_and_parameters("db", &vec![Literal::Numeric(0xFF)]).ok().unwrap().size()),
      (11, Directive::from_identifier_and_parameters("db", &vec![Literal::String("TEST_STRING")]).ok().unwrap().size()),
      (12, Directive::from_identifier_and_parameters("db", &vec![Literal::String("TEST_STRING"), Literal::Numeric(0x00)]).ok().unwrap().size())
    ];

    // Validate the expected size is obtained for each directive.
    for &(expected_size, size) in test_cases.iter() {
      assert_eq!(size, expected_size);
    }
  }

}
