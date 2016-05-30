
use assembler::parser::Literal;
use assembler::u12::*;

pub trait Emittable {

  /**
   @return The size of the emittable element in bytes.
   */
  fn size(&self) -> usize;

  /**
   Consuming conversion from an emittable element into an byte Vec.
   @return A Vec of bytes to be written into the output stream.
   */
  fn into_slice(self) -> Vec<u8>;

}

pub enum Directive {
  /// Directive to set the emit origin to the new address.
  Org(U12),
  /// Directive to emit the specified bytes.
  Db(Vec<u8>)
}

pub mod names {
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
  fn from_identifier_and_parameters(identifier: &'a str, arguments: &Vec<Literal>) -> Result<Directive,String> {
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

      let mut bytes = Vec::<u8>::new();

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
