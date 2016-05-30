
use assembler::assembler::Emittable;
use assembler::parser::Literal;
use assembler::u12::*;

#[derive(PartialEq, Eq, Debug)]
pub enum Directive {
  /// Directive to set the emit origin to the new address.
  Org(U12),
  /// Directive to emit the specified bytes.
  Db(Vec<u8>)
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
  pub fn from_identifier_and_parameters(identifier: &'a str, arguments: &Vec<Literal>) -> Result<Directive,String> {
    /*
     The 'org' directive is used to set the current output origin address. The directive requires
     a single numeric literal in the range $000-$FFF.
     */
    if identifier == names::ORG {
      if arguments.len() != 1 {
        return Err(format!("Incorrect number of parameters ({}) for directive .org, expecting 1.", arguments.len()));
      }

      return match arguments[0] {
        Literal::Numeric(a) if a <= 4095 => {
          Ok(Directive::Org(a.as_u12().unwrap()))
        }
        _ => {
          Err(format!("Directive .org requires 1 numeric literal in the range $000-$FFF."))
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
        return Err(format!("Incorrect number of parameters (0) for directive .db, expecting 1 or more."));
      }

      let mut bytes = Vec::new();

      // Convert all valid parameters into their byte literal values.
      for argument in arguments {
        match argument {
          &Literal::Numeric(value) => {
            if value > 0xFF {
              return Err(format!("All numeric parameters to .db must be 1-byte literals (${:X} > $FF)", value));
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

    Err(format!("Unrecognized directive .{}", identifier))
  }
}

impl Emittable for Directive {

  /**
   @return The size of the directive in the output stream in bytes.
   */
  fn size(&self) -> usize {
    match *self {
      Directive::Org(_)       => 0,
      Directive::Db(ref data) => data.len()
    }
  }

  /**
   Consuming conversion from a directive element into the byte representation in the output stream.
   @return A Vec of bytes to be written into the output stream for the directive.
   */
  fn into_slice(self) -> Vec<u8> {
    match self {
      Directive::Org(_)       => vec![],
      Directive::Db(data)     => data
    }
  }

}

// MARK: - Tests

#[cfg(test)]
mod tests {

  use assembler::assembler::Emittable;
  use assembler::parser::Literal;
  use assembler::u12;
  use assembler::u12::*;

  use super::*;

  #[test]
  fn test_validate_directive_semantics_for_org() {
    let mut params = vec![];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err("Incorrect number of parameters (0) for directive .org, expecting 1.".to_string()));
    params = vec![Literal::Numeric(1), Literal::Numeric(3)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err("Incorrect number of parameters (2) for directive .org, expecting 1.".to_string()));
    params = vec![Literal::Numeric(0x1000)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err("Directive .org requires 1 numeric literal in the range $000-$FFF.".to_string()));
    params = vec![Literal::String("TEST_STRING")];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Err("Directive .org requires 1 numeric literal in the range $000-$FFF.".to_string()));
    params = vec![Literal::Numeric(0xFFF)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Ok(Directive::Org(u12::MAX)));
    params = vec![Literal::Numeric(0x000)];
    assert_eq!(Directive::from_identifier_and_parameters("org", &params), Ok(Directive::Org(u12::MIN)));
  }

  #[test]
  fn test_validate_directive_semantics_for_db() {
    let mut params = vec![];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Err("Incorrect number of parameters (0) for directive .db, expecting 1 or more.".to_string()));
    params = vec![Literal::Numeric(0x100)];
    assert_eq!(Directive::from_identifier_and_parameters("db", &params), Err("All numeric parameters to .db must be 1-byte literals ($100 > $FF)".to_string()));
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
    assert_eq!(Directive::from_identifier_and_parameters("dw", &params), Err("Unrecognized directive .dw".to_string()));
    params = vec![Literal::Numeric(0x100)];
    assert_eq!(Directive::from_identifier_and_parameters("dw", &params), Err("Unrecognized directive .dw".to_string()));
  }

  #[test]
  fn test_size_of_directive_for_org() {
    let org_directive = Directive::from_identifier_and_parameters("org", &vec![Literal::Numeric(0xFFF)]).ok().unwrap();
    assert_eq!(org_directive.size(), 0);
  }

  #[test]
  fn test_size_of_directive_for_db() {
    let expected_sizes = [1, 11, 12];
    let directives = [
      Directive::from_identifier_and_parameters("db", &vec![Literal::Numeric(0xFF)]).ok().unwrap(),
      Directive::from_identifier_and_parameters("db", &vec![Literal::String("TEST_STRING")]).ok().unwrap(),
      Directive::from_identifier_and_parameters("db", &vec![Literal::String("TEST_STRING"), Literal::Numeric(0x00)]).ok().unwrap()
    ];

    // Validate the expected size is obtained for each directive.
    for (i, directive) in directives.iter().enumerate() {
      let size = directive.size();
      let expected_size = expected_sizes[i];
      assert_eq!(size, expected_size);
    }
  }

}
