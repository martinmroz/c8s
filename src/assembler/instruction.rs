//
// Copyright 2016 The c8s Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>.
// All files in the project carrying such notice may not be copied, modified, or 
// distributed except according to those terms.
//

use std::collections::HashMap;
use std::error;
use std::fmt;

use twelve_bit::u12::*;

use assembler::opcode::Opcode;
use assembler::parser::InstructionField;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Error {
  UnableToResolveLabel(String),
  ExpectingEightBitValue(usize),
  ExpectingFourBitValue(usize),
  NoMatchingFormat(String, Option<String>)
}

impl error::Error for Error {
  /// Returns a string slice with a general description of a directive error.
  /// No specific information is contained. To obtain a printable representation,
  /// use the `fmt::Display` attribute.
  fn description(&self) -> &str {
    match self {
      &Error::UnableToResolveLabel(_)           => "Unable to resolve label",
      &Error::ExpectingEightBitValue(_)         => "Found 12-bit numeric value, expecting 8-bit value",
      &Error::ExpectingFourBitValue(_)          => "Found 8/12-bit numeric value, expecting 4-bit value",
      &Error::NoMatchingFormat(_,_)             => "No matching instruction",
    }
  }
}

impl fmt::Display for Error {
  /// Formats the receiver for display purposes. Incorporates specific information
  /// relating to this particular error instance where applicable.
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Error::UnableToResolveLabel(ref name) => 
        write!(f, "Unable to resolve label {}.", name),
      &Error::ExpectingEightBitValue(actual) =>
        write!(f, "Found 12-bit numeric value ${:X}, expecting 8-bit value.", actual),
      &Error::ExpectingFourBitValue(actual) =>
        write!(f, "Found 8/12-bit numeric value ${:X}, expecting 4-bit value.", actual),
      &Error::NoMatchingFormat(ref mnemonic, Some(ref fields)) =>
        write!(f, "No matching instruction: {} {}.", mnemonic, fields),
      &Error::NoMatchingFormat(ref mnemonic, None) =>
        write!(f, "No matching instruction: {}.", mnemonic),
    }
  }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct Instruction(Opcode);

/**
 Resolves the label from the label map.
 @param label The name of the label (case-sensitive).
 @param label_map The mapping of labels to their resolved addresses.
 @return The address from the label map if defined, or an error describing the failure.
 */
fn resolve_label_with_map<'a>(label: &'a str, label_map: &HashMap<&'a str, U12>) -> Result<U12, Error> {
  label_map
    .get(label)
    .map(|value| *value)
    .ok_or(Error::UnableToResolveLabel(String::from(label)))
}

/**
 Converts a 12-bit (unsigned) literal value to 8-bits if possible to do so without data loss.
 @param literal 12-bit literal.
 @return A result consisting of the low 8 bits of the 12-bit literal or an explanation as to why conversion failed.
 */
fn numeric_literal_to_8_bit_field(literal: U12) -> Result<u8, Error> {
  literal
    .failable_into()
    .ok_or(Error::ExpectingEightBitValue(usize::from(literal)))
}

/**
 Converts a 12-bit (unsigned) literal value to 4-bits if possible to do so without data loss.
 @param literal 12-bit literal.
 @return A result consisting of the low 4 bits of the 12-bit literal or an explanation as to why conversion failed.
 */
fn numeric_literal_to_4_bit_field(literal: U12) -> Result<u8, Error> {
  if let Some(eight_bit) = literal.failable_into() {
    if eight_bit < 16 {
      return Ok(eight_bit);
    }
  }

  Err(Error::ExpectingFourBitValue(usize::from(literal)))
}

impl<'a> Instruction {

  /**
   Converts a mnemonic string and a list of associated fields into an opcode (with the help of a
   label resolution map). Produces an error message in the event of failure.
   @param mnemonic The (all lowercase) instruction mnemonic.
   @param fields A vector of instruction fields.
   @param label_map A reference to a map of labels to their defined address values.
   @return The assembled instruction if successful or a string describing the failure otherwise.
   */
  pub fn from_mnemonic_and_parameters(input_mnemonic: &'a str, fields: &Vec<InstructionField<'a>>, label_map: &HashMap<&'a str, U12>) -> Result<Self, Error> {
    let mut opcode: Option<Opcode> = None;

    // Mnemonics match case-insensitive.
    let lowercase_mnemonic_string = String::from(input_mnemonic).to_lowercase();
    let mnemonic = lowercase_mnemonic_string.as_str();

    // Mnemonics with zero parameters.
    if fields.len() == 0 {
      opcode = match mnemonic {
        "nop"     => Some(Opcode::NOP),
        "cls"     => Some(Opcode::CLS),
        "ret"     => Some(Opcode::RET),
        "trap"    => Some(Opcode::TRAP),
        "trapret" => Some(Opcode::TRAPRET),
        _         => None
      };
    }

    // Mnemonics with one parameter.
    if fields.len() == 1 {
      opcode = match (mnemonic, fields.get(0).unwrap()) {

        ("jp", &InstructionField::NumericLiteral(target)) => {
          Some(Opcode::JP { target: target })
        }

        // The 'jp' mnemonic may reference a label.
        ("jp", &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::JP { target: address })
        }

        ("call", &InstructionField::NumericLiteral(target)) => {
          Some(Opcode::CALL { target: target })
        }

        // The 'call' mnemonic may reference a label.
        ("call", &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::CALL { target: address })
        }

        ("shr", &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SHR { register_x: x, register_y: 0 })
        }

        ("shl", &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SHL { register_x: x, register_y: 0 })
        }

        ("skp", &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SE_K { register_x: x })
        }

        ("sknp", &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SNE_K { register_x: x })
        }

        _ => None
      }
    }

    // Mnemonics with two parameters.
    if fields.len() == 2 {
      opcode = match (mnemonic, fields.get(0).unwrap(), fields.get(1).unwrap()) {
        ("ld", &InstructionField::IndexRegister, &InstructionField::IndexRegisterIndirect) => {
          Some(Opcode::DEREF_I)
        }

        ("se", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::SE_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(value)) })
        }

        // The 'se Vx,NN' mnemonic may reference a label.
        ("se", &InstructionField::GeneralPurposeRegister(x), &InstructionField::Identifier(label)) => {
          let label_value = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::SE_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(label_value)) })
        }

        ("sne", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::SNE_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(value)) })
        }

        // The 'sne Vx,NN' mnemonic may reference a label.
        ("sne", &InstructionField::GeneralPurposeRegister(x), &InstructionField::Identifier(label)) => {
          let label_value = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::SNE_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(label_value)) })
        }

        ("se", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::SE_REGISTER { register_x: x, register_y: y })
        }

        ("sne", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::SNE_REGISTER { register_x: x, register_y: y })
        }

        ("ld", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::LD_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(value)) })
        }

        // The 'ld Vx,NN' mnemonic may reference a label.
        ("ld", &InstructionField::GeneralPurposeRegister(x), &InstructionField::Identifier(label)) => {
          let label_value = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::LD_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(label_value)) })
        }

        ("add", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::ADD_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(value)) })
        }

        // The 'add Vx,NN' mnemonic may reference a label.
        ("add", &InstructionField::GeneralPurposeRegister(x), &InstructionField::Identifier(label)) => {
          let label_value = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::ADD_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(label_value)) })
        }

        ("ld", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::LD_REGISTER { register_x: x, register_y: y })
        }

        ("or", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::OR { register_x: x, register_y: y })
        }

        ("and", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::AND { register_x: x, register_y: y })
        }

        ("xor", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::XOR { register_x: x, register_y: y })
        }

        ("add", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::ADD_REGISTER { register_x: x, register_y: y })
        }

        ("sub", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::SUB { register_x: x, register_y: y })
        }

        ("subn", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::SUBN { register_x: x, register_y: y })
        }

        ("ld", &InstructionField::IndexRegister, &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::LD_I { value: value })
        }

        // The 'ld I,NNN' mnemonic may reference a label.
        ("ld", &InstructionField::IndexRegister, &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::LD_I { value: address })
        }

        ("jp", &InstructionField::GeneralPurposeRegister(0), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::JP_V0 { value: value })
        }

        // The 'jp V0,NNN' mnemonic may reference a label.
        ("jp", &InstructionField::GeneralPurposeRegister(0), &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::JP_V0 { value: address })
        }

        ("rnd", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::RND { register_x: x, mask: try!(numeric_literal_to_8_bit_field(value)) })
        }

        // The 'rnd Vx,NN' mnemonic may reference a label.
        ("rnd", &InstructionField::GeneralPurposeRegister(x), &InstructionField::Identifier(label)) => {
          let label_value = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::RND { register_x: x, mask: try!(numeric_literal_to_8_bit_field(label_value)) })
        }

        ("ld", &InstructionField::GeneralPurposeRegister(x), &InstructionField::DelayTimer) => {
          Some(Opcode::LD_X_DT { register_x: x })
        }

        ("ld", &InstructionField::GeneralPurposeRegister(x), &InstructionField::KeypadRegister) => {
          Some(Opcode::LD_X_K { register_x: x })
        }

        ("ld", &InstructionField::DelayTimer, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::LD_DT_X { register_x: x })
        }

        ("ld", &InstructionField::SoundTimer, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::LD_ST_X { register_x: x })
        }

        ("add", &InstructionField::IndexRegister, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::ADD_I_X { register_x: x })
        }

        ("ld", &InstructionField::Sprite, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SPRITE_I { register_x: x })
        }

        ("ld", &InstructionField::Bcd, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::BCD_I { register_x: x })
        }

        ("ld", &InstructionField::IndexRegisterIndirect, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SAVE_I { register_x: x })
        }

        ("ld", &InstructionField::GeneralPurposeRegister(x), &InstructionField::IndexRegisterIndirect) => {
          Some(Opcode::RESTORE_I { register_x: x })
        }

        _ => None
      };
    }

    // Mnemonics with three parameter.
    if fields.len() == 3 {
      let tuple = (mnemonic, fields.get(0).unwrap(), fields.get(1).unwrap(), fields.get(2).unwrap());
      if let (
        "drw", 
        &InstructionField::GeneralPurposeRegister(x), 
        &InstructionField::GeneralPurposeRegister(y),
        &InstructionField::NumericLiteral(value)) = tuple {
        opcode = Some(Opcode::DRW {register_x: x, register_y: y, bytes: try!(numeric_literal_to_4_bit_field(value)) } );
      }

      // The 'drw Vx,Vy,N' mnemonic may reference a label.
      if let (
        "drw",
        &InstructionField::GeneralPurposeRegister(x),
        &InstructionField::GeneralPurposeRegister(y),
        &InstructionField::Identifier(label)) = tuple {
        let label_value = try!(resolve_label_with_map(label, label_map));
        opcode = Some(Opcode::DRW {register_x: x, register_y: y, bytes: try!(numeric_literal_to_4_bit_field(label_value)) } );
      }
    }

    opcode
      // Create an Instruction out of the Opcode in the event of success.
      .map(|assembled_opcode| {
        Instruction(assembled_opcode)
      })
      // Generate a NoMatchingFormat error in the event of failure.
      .ok_or_else(|| {
        Error::NoMatchingFormat(String::from(input_mnemonic), match fields.len() {
          0 => None,
          _ => {
            let field_strings = fields.iter().map(|field| format!("{}", field));
            let field_description = field_strings.collect::<Vec<_>>().join(", ");
            Some(field_description)
          }
        })
      })
  }

  /**
   @return The size of the instruction in the output stream in bytes (always 2).
   */
  pub fn size(&self) -> usize { 2 }

  /**
   Consuming conversion from a directive element into the byte representation in the output stream.
   @return A Vec of bytes to be written into the output stream for the directive.
   */
  pub fn into_vec(self) -> Vec<u8> {
    let instruction: u16 = self.0.as_instruction();
    vec![
      ((instruction & 0xFF00) >> 8) as u8,
      ((instruction & 0x00FF) >> 0) as u8
    ]
  }

}

// MARK: - Tests

#[cfg(test)]
mod tests {

  use std::collections::HashMap;

  use twelve_bit::u12;
  use twelve_bit::u12::*;

  use assembler::opcode::Opcode;
  use assembler::parser::InstructionField;

  use super::*;

  #[test]
  fn test_error_display() {
    assert_eq!(format!("{}", Error::UnableToResolveLabel(String::from("TEST_LABEL"))),
      "Unable to resolve label TEST_LABEL.");
    assert_eq!(format!("{}", Error::ExpectingEightBitValue(0x100)),
      "Found 12-bit numeric value $100, expecting 8-bit value.");
    assert_eq!(format!("{}", Error::ExpectingFourBitValue(0x10)),
      "Found 8/12-bit numeric value $10, expecting 4-bit value.");
    assert_eq!(format!("{}", Error::NoMatchingFormat(String::from("jp"), Some(String::from("register:0")))),
      "No matching instruction: jp register:0.");
    assert_eq!(format!("{}", Error::NoMatchingFormat(String::from("jp"), None)),
      "No matching instruction: jp.");
  }

  #[test]
  fn test_nop() {
    let empty_map = HashMap::new();
    let nop = Instruction::from_mnemonic_and_parameters("nop", &vec![], &empty_map).unwrap();
    assert_eq!(nop.size(), 2);
    assert_eq!(nop.0, Opcode::NOP);
  }

  #[test]
  fn test_cls() {
    let empty_map = HashMap::new();
    let cls = Instruction::from_mnemonic_and_parameters("cls", &vec![], &empty_map).unwrap();
    assert_eq!(cls.size(), 2);
    assert_eq!(cls.0, Opcode::CLS);
  }

  #[test]
  fn test_deref_i() {
    let empty_map = HashMap::new();
    let ret = Instruction::from_mnemonic_and_parameters("ld", &vec![
      InstructionField::IndexRegister,
      InstructionField::IndexRegisterIndirect], 
      &empty_map
    ).unwrap();
    assert_eq!(ret.size(), 2);
    assert_eq!(ret.0, Opcode::DEREF_I);
  }

  #[test]
  fn test_ret() {
    let empty_map = HashMap::new();
    let ret = Instruction::from_mnemonic_and_parameters("ret", &vec![], &empty_map).unwrap();
    assert_eq!(ret.size(), 2);
    assert_eq!(ret.0, Opcode::RET);
  }

  #[test]
  fn test_trapret() {
    let empty_map = HashMap::new();
    let trapret = Instruction::from_mnemonic_and_parameters("trapret", &vec![], &empty_map).unwrap();
    assert_eq!(trapret.size(), 2);
    assert_eq!(trapret.0, Opcode::TRAPRET);
  }

  #[test]
  fn test_trap() {
    let empty_map = HashMap::new();
    let trap = Instruction::from_mnemonic_and_parameters("trap", &vec![], &empty_map).unwrap();
    assert_eq!(trap.size(), 2);
    assert_eq!(trap.0, Opcode::TRAP);
  }

  #[test]
  fn test_jp() {
    let empty_map = HashMap::new();
    let mut defined_map = HashMap::new();
    defined_map.insert("TEST_LABEL", u12::MAX);

    // Failure Mode 1: No Parameters.
    let invalid_jp = Instruction::from_mnemonic_and_parameters("jp", &vec![], &empty_map);
    assert_eq!(invalid_jp.is_err(), true);
    assert_eq!(invalid_jp.unwrap_err(), Error::NoMatchingFormat(String::from("jp"), None));

    // Failure Mode 2: Undefined Label.
    let label_field = InstructionField::Identifier("TEST_LABEL");
    let invalid_jp_nl = Instruction::from_mnemonic_and_parameters("jp", &vec![label_field.clone()], &empty_map);
    assert_eq!(invalid_jp_nl.is_err(), true);
    assert_eq!(invalid_jp_nl.unwrap_err(), Error::UnableToResolveLabel(String::from("TEST_LABEL")));

    // Success 1: Literal 12-bit Numeric.
    let literal_field = InstructionField::NumericLiteral(u12::MAX);
    let jp_literal = Instruction::from_mnemonic_and_parameters("jp", &vec![literal_field], &empty_map).unwrap();
    assert_eq!(jp_literal.size(), 2);
    assert_eq!(jp_literal.0, Opcode::JP { target: u12::MAX });

    // Success 2: Defined 12-bit Label.
    let jp_label = Instruction::from_mnemonic_and_parameters("jp", &vec![label_field], &defined_map).unwrap();
    assert_eq!(jp_label.size(), 2);
    assert_eq!(jp_label.0, Opcode::JP { target: u12::MAX });
  }

  #[test]
  fn test_jp_v0() {
    let empty_map = HashMap::new();
    let mut defined_map = HashMap::new();
    defined_map.insert("TEST_LABEL", u12::MAX);

    let v0_field = InstructionField::GeneralPurposeRegister(0);

    // Failure Mode 1: Just v0.
    let invalid_jp_v0 = Instruction::from_mnemonic_and_parameters("jp", &vec![v0_field.clone()], &empty_map);
    assert_eq!(invalid_jp_v0.is_err(), true);
    assert_eq!(invalid_jp_v0.unwrap_err(), Error::NoMatchingFormat(String::from("jp"), Some(String::from("register:0"))));

    // Failure Mode 2: Undefined Label.
    let label_field = InstructionField::Identifier("TEST_LABEL");
    let invalid_jp_v0_nl = Instruction::from_mnemonic_and_parameters("jp", &vec![v0_field.clone(), label_field.clone()], &empty_map);
    assert_eq!(invalid_jp_v0_nl.is_err(), true);
    assert_eq!(invalid_jp_v0_nl.unwrap_err(), Error::UnableToResolveLabel(String::from("TEST_LABEL")));

    // Success 1: Literal 12-bit Numeric.
    let literal_field = InstructionField::NumericLiteral(u12::MAX);
    let jp_v0_literal = Instruction::from_mnemonic_and_parameters("jp", &vec![v0_field.clone(), literal_field], &empty_map).unwrap();
    assert_eq!(jp_v0_literal.size(), 2);
    assert_eq!(jp_v0_literal.0, Opcode::JP_V0 { value: u12::MAX });

    // Success 2: Defined 12-bit Label.
    let jp_v0_label = Instruction::from_mnemonic_and_parameters("jp", &vec![v0_field, label_field], &defined_map).unwrap();
    assert_eq!(jp_v0_label.size(), 2);
    assert_eq!(jp_v0_label.0, Opcode::JP_V0 { value: u12::MAX });
  }

  #[test]
  fn test_call() {
    let empty_map = HashMap::new();
    let mut defined_map = HashMap::new();
    defined_map.insert("TEST_LABEL", u12::MAX);

    // Failure Mode 1: No Parameters.
    let invalid_call = Instruction::from_mnemonic_and_parameters("call", &vec![], &empty_map);
    assert_eq!(invalid_call.is_err(), true);
    assert_eq!(invalid_call.unwrap_err(), Error::NoMatchingFormat(String::from("call"), None));

    // Failure Mode 2: Undefined Label.
    let label_field = InstructionField::Identifier("TEST_LABEL");
    let invalid_call_nl = Instruction::from_mnemonic_and_parameters("call", &vec![label_field.clone()], &empty_map);
    assert_eq!(invalid_call_nl.is_err(), true);
    assert_eq!(invalid_call_nl.unwrap_err(), Error::UnableToResolveLabel(String::from("TEST_LABEL")));

    // Success 1: Literal 12-bit Numeric.
    let literal_field = InstructionField::NumericLiteral(u12::MAX);
    let call_literal = Instruction::from_mnemonic_and_parameters("call", &vec![literal_field], &empty_map).unwrap();
    assert_eq!(call_literal.size(), 2);
    assert_eq!(call_literal.0, Opcode::CALL { target: u12::MAX });

    // Success 2: Defined 12-bit Label.
    let call_label = Instruction::from_mnemonic_and_parameters("call", &vec![label_field], &defined_map).unwrap();
    assert_eq!(call_label.size(), 2);
    assert_eq!(call_label.0, Opcode::CALL { target: u12::MAX });
  }

  #[test]
  fn test_se_immediate_and_register_and_keypad() {
    let empty_map = HashMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);

    // Skip Next If vX == vY.
    let se_rr = Instruction::from_mnemonic_and_parameters("se", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(se_rr.size(), 2);
    assert_eq!(se_rr.0, Opcode::SE_REGISTER { register_x: 1, register_y: 2 });

    // Skip next if vX == nn.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xFF));
    let se_imm = Instruction::from_mnemonic_and_parameters("se", &vec![register_field_1, immediate_field], &empty_map).unwrap();
    assert_eq!(se_imm.size(), 2);
    assert_eq!(se_imm.0, Opcode::SE_IMMEDIATE { register_x: 1, value: 0xFF });

    // Immediate fields for se vX, $nn is limited to 8 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(u12![0x100]);
    let invalid_se_imm = Instruction::from_mnemonic_and_parameters("se", &vec![register_field_1, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_se_imm.is_err(), true);
    assert_eq!(invalid_se_imm.unwrap_err(), Error::ExpectingEightBitValue(0x100));

    // Skip next if key represented by vX is pressed.
    let se_k = Instruction::from_mnemonic_and_parameters("skp", &vec![register_field_1], &empty_map).unwrap();
    assert_eq!(se_k.size(), 2);
    assert_eq!(se_k.0, Opcode::SE_K { register_x: 1 });
  }

  #[test]
  fn test_se_immediate_label() {
    let mut label_map = HashMap::new();
    label_map.insert("TWELVE_BITS", u12::MAX);
    label_map.insert("EIGHT_BITS", u12![255]);
    label_map.insert("FOUR_BITS", u12![15]);

    // Skip next if Vx == LABEL:nn with valid 8-bit parameter.
    let se_label8 = Instruction::from_mnemonic_and_parameters("se", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::Identifier("EIGHT_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(se_label8.size(), 2);
    assert_eq!(se_label8.0, Opcode::SE_IMMEDIATE { register_x: 1, value: 0xFF });

    // Skip next if Vx == LABEL:nn with valid 4-bit parameter.
    let se_label4 = Instruction::from_mnemonic_and_parameters("se", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("FOUR_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(se_label4.size(), 2);
    assert_eq!(se_label4.0, Opcode::SE_IMMEDIATE { register_x: 2, value: 0x0F });

    // Skip next if Vx == LABEL:nn with invalid 12-bit parameter.
    let se_invalid_label_value_error = Instruction::from_mnemonic_and_parameters("se", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("TWELVE_BITS")],
      &label_map
    ).unwrap_err();
    assert_eq!(se_invalid_label_value_error, Error::ExpectingEightBitValue(0xFFF));

    // Skip next if Vx == LABEL:nn with invalid label name.
    let se_invalid_label_name_error = Instruction::from_mnemonic_and_parameters("se", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("INVALID")],
      &label_map
    ).unwrap_err();
    assert_eq!(se_invalid_label_name_error, Error::UnableToResolveLabel(String::from("INVALID")));
  }

  #[test]
  fn test_sne_immediate_and_register_and_keypad() {
    let empty_map = HashMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);

    // Skip Next If vX != vY.
    let sne_rr = Instruction::from_mnemonic_and_parameters("sne", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(sne_rr.size(), 2);
    assert_eq!(sne_rr.0, Opcode::SNE_REGISTER { register_x: 1, register_y: 2 });

    // Skip next if vX != nn.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xFF));
    let sne_imm = Instruction::from_mnemonic_and_parameters("sne", &vec![register_field_1, immediate_field], &empty_map).unwrap();
    assert_eq!(sne_imm.size(), 2);
    assert_eq!(sne_imm.0, Opcode::SNE_IMMEDIATE { register_x: 1, value: 0xFF });

    // Immediate fields for sne vX, $nn is limited to 8 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(u12![0x100]);
    let invalid_sne_imm = Instruction::from_mnemonic_and_parameters("sne", &vec![register_field_1, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_sne_imm.is_err(), true);
    assert_eq!(invalid_sne_imm.unwrap_err(), Error::ExpectingEightBitValue(0x100));

    // Skip next if key represented by vX is NOT pressed.
    let sne_k = Instruction::from_mnemonic_and_parameters("sknp", &vec![register_field_1], &empty_map).unwrap();
    assert_eq!(sne_k.size(), 2);
    assert_eq!(sne_k.0, Opcode::SNE_K { register_x: 1 });
  }

  #[test]
  fn test_sne_immediate_label() {
    let mut label_map = HashMap::new();
    label_map.insert("TWELVE_BITS", u12::MAX);
    label_map.insert("EIGHT_BITS", u12![255]);
    label_map.insert("FOUR_BITS", u12![15]);

    // Skip next if Vx != LABEL:nn with valid 8-bit parameter.
    let sne_label8 = Instruction::from_mnemonic_and_parameters("sne", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::Identifier("EIGHT_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(sne_label8.size(), 2);
    assert_eq!(sne_label8.0, Opcode::SNE_IMMEDIATE { register_x: 1, value: 0xFF });

    // Skip next if Vx != LABEL:nn with valid 4-bit parameter.
    let sne_label4 = Instruction::from_mnemonic_and_parameters("sne", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("FOUR_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(sne_label4.size(), 2);
    assert_eq!(sne_label4.0, Opcode::SNE_IMMEDIATE { register_x: 2, value: 0x0F });

    // Skip next if Vx != LABEL:nn with invalid 12-bit parameter.
    let sne_invalid_label_value_error = Instruction::from_mnemonic_and_parameters("sne", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("TWELVE_BITS")],
      &label_map
    ).unwrap_err();
    assert_eq!(sne_invalid_label_value_error, Error::ExpectingEightBitValue(0xFFF));

    // Skip next if Vx != LABEL:nn with invalid label name.
    let sne_invalid_label_name_error = Instruction::from_mnemonic_and_parameters("sne", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("INVALID")],
      &label_map
    ).unwrap_err();
    assert_eq!(sne_invalid_label_name_error, Error::UnableToResolveLabel(String::from("INVALID")));
  }

  #[test]
  fn test_ld_immediate_and_register_and_index() {
    let empty_map = HashMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);

    // Load vX := vY.
    let ld_rr = Instruction::from_mnemonic_and_parameters("ld", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(ld_rr.size(), 2);
    assert_eq!(ld_rr.0, Opcode::LD_REGISTER { register_x: 1, register_y: 2 });

    // Load vX := imm8.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xFF));
    let ld_imm = Instruction::from_mnemonic_and_parameters("ld", &vec![register_field_1, immediate_field], &empty_map).unwrap();
    assert_eq!(ld_imm.size(), 2);
    assert_eq!(ld_imm.0, Opcode::LD_IMMEDIATE { register_x: 1, value: 0xFF });

    // Immediate fields for ld vX, $nn is limited to 8 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(u12![0x100]);
    let invalid_ld_imm = Instruction::from_mnemonic_and_parameters("ld", &vec![register_field_1, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_ld_imm.is_err(), true);
    assert_eq!(invalid_ld_imm.unwrap_err(), Error::ExpectingEightBitValue(0x100));
  }

  #[test]
  fn test_ld_immediate_label() {
    let mut label_map = HashMap::new();
    label_map.insert("TWELVE_BITS", u12::MAX);
    label_map.insert("EIGHT_BITS", u12![255]);
    label_map.insert("FOUR_BITS", u12![15]);

    // Load the constant value LABEL:nn into Vx with valid 8-bit parameter.
    let ld_label8 = Instruction::from_mnemonic_and_parameters("ld", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::Identifier("EIGHT_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(ld_label8.size(), 2);
    assert_eq!(ld_label8.0, Opcode::LD_IMMEDIATE { register_x: 1, value: 0xFF });

    // Load the constant value LABEL:nn into Vx with valid 4-bit parameter.
    let ld_label4 = Instruction::from_mnemonic_and_parameters("ld", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("FOUR_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(ld_label4.size(), 2);
    assert_eq!(ld_label4.0, Opcode::LD_IMMEDIATE { register_x: 2, value: 0x0F });

    // Load the constant value LABEL:nn into Vx with invalid 12-bit parameter.
    let ld_invalid_label_value_error = Instruction::from_mnemonic_and_parameters("ld", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("TWELVE_BITS")],
      &label_map
    ).unwrap_err();
    assert_eq!(ld_invalid_label_value_error, Error::ExpectingEightBitValue(0xFFF));

    // Load the constant value LABEL:nn into Vx with invalid label name.
    let ld_invalid_label_name_error = Instruction::from_mnemonic_and_parameters("ld", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("INVALID")],
      &label_map
    ).unwrap_err();
    assert_eq!(ld_invalid_label_name_error, Error::UnableToResolveLabel(String::from("INVALID")));
  }

  #[test]
  fn test_ld_index() {
    let empty_map = HashMap::new();
    let mut defined_map = HashMap::new();
    defined_map.insert("TEST_LABEL", u12::MAX);

    // Failure Mode: Undefined Label.
    let index_register_field = InstructionField::IndexRegister;
    let label_field = InstructionField::Identifier("TEST_LABEL");
    let invalid_ld_i = Instruction::from_mnemonic_and_parameters("ld", &vec![index_register_field.clone(), label_field.clone()], &empty_map);
    assert_eq!(invalid_ld_i.is_err(), true);
    assert_eq!(invalid_ld_i.unwrap_err(), Error::UnableToResolveLabel(String::from("TEST_LABEL")));

    // Success 1: Literal 12-bit Numeric.
    let literal_field = InstructionField::NumericLiteral(u12::MAX);
    let ld_i_literal = Instruction::from_mnemonic_and_parameters("ld", &vec![index_register_field.clone(), literal_field], &empty_map).unwrap();
    assert_eq!(ld_i_literal.size(), 2);
    assert_eq!(ld_i_literal.0, Opcode::LD_I { value: u12::MAX });

    // Success 2: Defined 12-bit Label.
    let ld_i_label = Instruction::from_mnemonic_and_parameters("ld", &vec![index_register_field, label_field], &defined_map).unwrap();
    assert_eq!(ld_i_label.size(), 2);
    assert_eq!(ld_i_label.0, Opcode::LD_I { value: u12::MAX });
  }

  #[test]
  fn test_add_immediate_and_register_and_index() {
    let empty_map = HashMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);
    let index_register_field = InstructionField::IndexRegister;

    // Add vX += vY.
    let add_rr = Instruction::from_mnemonic_and_parameters("add", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(add_rr.size(), 2);
    assert_eq!(add_rr.0, Opcode::ADD_REGISTER { register_x: 1, register_y: 2 });

    // Add vX += imm8.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xFF));
    let add_imm = Instruction::from_mnemonic_and_parameters("add", &vec![register_field_1, immediate_field], &empty_map).unwrap();
    assert_eq!(add_imm.size(), 2);
    assert_eq!(add_imm.0, Opcode::ADD_IMMEDIATE { register_x: 1, value: 0xFF });

    // Immediate fields for add vX, $nn is limited to 8 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(u12![0x100]);
    let invalid_add_imm = Instruction::from_mnemonic_and_parameters("add", &vec![register_field_1, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_add_imm.is_err(), true);
    assert_eq!(invalid_add_imm.unwrap_err(), Error::ExpectingEightBitValue(0x100));

    // Add I += vX.
    let add_i = Instruction::from_mnemonic_and_parameters("add", &vec![index_register_field, register_field_2], &empty_map).unwrap();
    assert_eq!(add_i.size(), 2);
    assert_eq!(add_i.0, Opcode::ADD_I_X { register_x: 2 });
  }

  #[test]
  fn test_add_immediate_label() {
    let mut label_map = HashMap::new();
    label_map.insert("TWELVE_BITS", u12::MAX);
    label_map.insert("EIGHT_BITS", u12![255]);
    label_map.insert("FOUR_BITS", u12![15]);

    // Add the 8-bit constant to a register.
    let add_label8 = Instruction::from_mnemonic_and_parameters("add", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::Identifier("EIGHT_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(add_label8.size(), 2);
    assert_eq!(add_label8.0, Opcode::ADD_IMMEDIATE { register_x: 1, value: 0xFF });

    // Add the 4-bit constant to a register.
    let add_label4 = Instruction::from_mnemonic_and_parameters("add", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("FOUR_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(add_label4.size(), 2);
    assert_eq!(add_label4.0, Opcode::ADD_IMMEDIATE { register_x: 2, value: 0x0F });

    // Add the (invalid) 12-bit constant to a register.
    let add_invalid_label_value_error = Instruction::from_mnemonic_and_parameters("add", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("TWELVE_BITS")],
      &label_map
    ).unwrap_err();
    assert_eq!(add_invalid_label_value_error, Error::ExpectingEightBitValue(0xFFF));

    // Attempt to reference an invalid label name.
    let add_invalid_label_name_error = Instruction::from_mnemonic_and_parameters("add", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("INVALID")],
      &label_map
    ).unwrap_err();
    assert_eq!(add_invalid_label_name_error, Error::UnableToResolveLabel(String::from("INVALID")));
  }

  #[test]
  fn test_and_or_xor_sub_subn_shr_shl() {
    let empty_map = HashMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);

    // And vX & vY.
    let and = Instruction::from_mnemonic_and_parameters("and", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(and.size(), 2);
    assert_eq!(and.0, Opcode::AND { register_x: 1, register_y: 2 });

    // Or vX | vY.
    let or = Instruction::from_mnemonic_and_parameters("or", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(or.size(), 2);
    assert_eq!(or.0, Opcode::OR { register_x: 1, register_y: 2 });

    // Xor vX ^ vY.
    let xor = Instruction::from_mnemonic_and_parameters("xor", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(xor.size(), 2);
    assert_eq!(xor.0, Opcode::XOR { register_x: 1, register_y: 2 });

    // Sub vX - vY.
    let sub = Instruction::from_mnemonic_and_parameters("sub", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(sub.size(), 2);
    assert_eq!(sub.0, Opcode::SUB { register_x: 1, register_y: 2 });

    // Subn vY - vX.
    let subn = Instruction::from_mnemonic_and_parameters("subn", &vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(subn.size(), 2);
    assert_eq!(subn.0, Opcode::SUBN { register_x: 1, register_y: 2 });

    // Shr vX.
    let shr = Instruction::from_mnemonic_and_parameters("shr", &vec![register_field_1], &empty_map).unwrap();
    assert_eq!(shr.size(), 2);
    assert_eq!(shr.0, Opcode::SHR { register_x: 1, register_y: 0 });

    // Shl vX.
    let shl = Instruction::from_mnemonic_and_parameters("shl", &vec![register_field_1], &empty_map).unwrap();
    assert_eq!(shl.size(), 2);
    assert_eq!(shl.0, Opcode::SHL { register_x: 1, register_y: 0 });
  }

  #[test]
  fn test_rnd() {
    let empty_map = HashMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);

    // Rnd vX, imm8.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xFF));
    let rnd_imm = Instruction::from_mnemonic_and_parameters("rnd", &vec![register_field_1, immediate_field], &empty_map).unwrap();
    assert_eq!(rnd_imm.size(), 2);
    assert_eq!(rnd_imm.0, Opcode::RND { register_x: 1, mask: 0xFF });

    // Immediate fields for rnd vX, $nn is limited to 8 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(u12![0x100]);
    let invalid_rnd_imm = Instruction::from_mnemonic_and_parameters("rnd", &vec![register_field_1, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_rnd_imm.is_err(), true);
    assert_eq!(invalid_rnd_imm.unwrap_err(), Error::ExpectingEightBitValue(0x100));
  }

  #[test]
  fn test_rnd_immediate_label() {
    let mut label_map = HashMap::new();
    label_map.insert("TWELVE_BITS", u12::MAX);
    label_map.insert("EIGHT_BITS", u12![255]);
    label_map.insert("FOUR_BITS", u12![15]);

    // Random with 8-bit immediate label.
    let rnd_label8 = Instruction::from_mnemonic_and_parameters("rnd", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::Identifier("EIGHT_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(rnd_label8.size(), 2);
    assert_eq!(rnd_label8.0, Opcode::RND { register_x: 1, mask: 0xFF });

    // Random with 4-bit immediate label.
    let rnd_label4 = Instruction::from_mnemonic_and_parameters("rnd", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("FOUR_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(rnd_label4.size(), 2);
    assert_eq!(rnd_label4.0, Opcode::RND { register_x: 2, mask: 0x0F });

    // Load the constant value LABEL:nn into Vx with invalid 12-bit parameter.
    let rnd_invalid_label_value_error = Instruction::from_mnemonic_and_parameters("rnd", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("TWELVE_BITS")],
      &label_map
    ).unwrap_err();
    assert_eq!(rnd_invalid_label_value_error, Error::ExpectingEightBitValue(0xFFF));

    // Load the constant value LABEL:nn into Vx with invalid label name.
    let rnd_invalid_label_name_error = Instruction::from_mnemonic_and_parameters("rnd", &vec![
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("INVALID")],
      &label_map
    ).unwrap_err();
    assert_eq!(rnd_invalid_label_name_error, Error::UnableToResolveLabel(String::from("INVALID")));
  }

  #[test]
  fn test_drw() {
    let empty_map = HashMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);

    // Drw vX, vY, imm4.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xF));
    let drw = Instruction::from_mnemonic_and_parameters("drw", &vec![register_field_1, register_field_2, immediate_field], &empty_map).unwrap();
    assert_eq!(drw.size(), 2);
    assert_eq!(drw.0, Opcode::DRW { register_x: 1, register_y: 2, bytes: 15 });

    // Immediate fields for drw vX, vY, $n is limited to 4 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(U12::from(0x10));
    let invalid_drw = Instruction::from_mnemonic_and_parameters("drw", &vec![register_field_1, register_field_2, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_drw.is_err(), true);
    assert_eq!(invalid_drw.unwrap_err(), Error::ExpectingFourBitValue(0x10));
  }

  #[test]
  fn test_drw_immediate_label() {
    let mut label_map = HashMap::new();
    label_map.insert("TWELVE_BITS", u12::MAX);
    label_map.insert("EIGHT_BITS", u12![255]);
    label_map.insert("FOUR_BITS", u12![15]);

    // Draw sprite with valid 4-bit parameter number of bytes.
    let drw_label4 = Instruction::from_mnemonic_and_parameters("drw", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("FOUR_BITS")],
      &label_map
    ).unwrap();
    assert_eq!(drw_label4.size(), 2);
    assert_eq!(drw_label4.0, Opcode::DRW { register_x: 1, register_y: 2, bytes: 15 });

    // Draw sprite with invalid 8-bit parameter number of bytes.
    let drw_invalid_label_value_12_error = Instruction::from_mnemonic_and_parameters("drw", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("TWELVE_BITS")],
      &label_map
    ).unwrap_err();
    assert_eq!(drw_invalid_label_value_12_error, Error::ExpectingFourBitValue(0xFFF));

    // Draw sprite with invalid 8-bit parameter number of bytes.
    let drw_invalid_label_value_8_error = Instruction::from_mnemonic_and_parameters("drw", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("EIGHT_BITS")],
      &label_map
    ).unwrap_err();
    assert_eq!(drw_invalid_label_value_8_error, Error::ExpectingFourBitValue(0xFF));

    // Draw sprite with invalid label name.
    let drw_invalid_label_name_error = Instruction::from_mnemonic_and_parameters("drw", &vec![
      InstructionField::GeneralPurposeRegister(1),
      InstructionField::GeneralPurposeRegister(2),
      InstructionField::Identifier("INVALID")],
      &label_map
    ).unwrap_err();
    assert_eq!(drw_invalid_label_name_error, Error::UnableToResolveLabel(String::from("INVALID")));
  }

  #[test]
  fn test_ld_delay_and_sound_timer_and_keypad() {
    let empty_map = HashMap::new();
    let register_field = InstructionField::GeneralPurposeRegister(15);
    let sound_timer_field = InstructionField::SoundTimer;
    let delay_timer_field = InstructionField::DelayTimer;
    let keypad_field = InstructionField::KeypadRegister;

    // Ld vX := DT.
    let ld_vx_dt = Instruction::from_mnemonic_and_parameters("ld", &vec![register_field, delay_timer_field], &empty_map).unwrap();
    assert_eq!(ld_vx_dt.size(), 2);
    assert_eq!(ld_vx_dt.0, Opcode::LD_X_DT { register_x: 15 });

    // Ld vX := K.
    let ld_vx_k = Instruction::from_mnemonic_and_parameters("ld", &vec![register_field, keypad_field], &empty_map).unwrap();
    assert_eq!(ld_vx_k.size(), 2);
    assert_eq!(ld_vx_k.0, Opcode::LD_X_K { register_x: 15 });

    // Ld DT := vX.
    let ld_dt_vx = Instruction::from_mnemonic_and_parameters("ld", &vec![delay_timer_field, register_field], &empty_map).unwrap();
    assert_eq!(ld_dt_vx.size(), 2);
    assert_eq!(ld_dt_vx.0, Opcode::LD_DT_X { register_x: 15 });

    // Ld ST := vX.
    let ld_st_vx = Instruction::from_mnemonic_and_parameters("ld", &vec![sound_timer_field, register_field], &empty_map).unwrap();
    assert_eq!(ld_st_vx.size(), 2);
    assert_eq!(ld_st_vx.0, Opcode::LD_ST_X { register_x: 15 });
  }

  #[test]
  fn test_sprite() {
    let empty_map = HashMap::new();
    let register_field = InstructionField::GeneralPurposeRegister(7);
    let sprite_field = InstructionField::Sprite;

    // ld f, vX.
    let sprite_i = Instruction::from_mnemonic_and_parameters("ld", &vec![sprite_field, register_field], &empty_map).unwrap();
    assert_eq!(sprite_i.size(), 2);
    assert_eq!(sprite_i.0, Opcode::SPRITE_I { register_x: 7 });
  }

  #[test]
  fn test_bcd() {
    let empty_map = HashMap::new();
    let register_field = InstructionField::GeneralPurposeRegister(7);
    let bcd_field = InstructionField::Bcd;

    // ld b, vX.
    let bcd_i = Instruction::from_mnemonic_and_parameters("ld", &vec![bcd_field, register_field], &empty_map).unwrap();
    assert_eq!(bcd_i.size(), 2);
    assert_eq!(bcd_i.0, Opcode::BCD_I { register_x: 7 });
  }

  #[test]
  fn test_save() {
    let empty_map = HashMap::new();
    let register_field = InstructionField::GeneralPurposeRegister(7);
    let index_register_indirect_field = InstructionField::IndexRegisterIndirect;

    // ld [i], vX.
    let save_i = Instruction::from_mnemonic_and_parameters("ld", &vec![index_register_indirect_field, register_field], &empty_map).unwrap();
    assert_eq!(save_i.size(), 2);
    assert_eq!(save_i.0, Opcode::SAVE_I { register_x: 7 });
  }

  #[test]
  fn test_restore() {
    let empty_map = HashMap::new();
    let register_field = InstructionField::GeneralPurposeRegister(7);
    let index_register_indirect_field = InstructionField::IndexRegisterIndirect;

    // ld vX, [i].
    let restore_i = Instruction::from_mnemonic_and_parameters("ld", &vec![register_field, index_register_indirect_field], &empty_map).unwrap();
    assert_eq!(restore_i.size(), 2);
    assert_eq!(restore_i.0, Opcode::RESTORE_I { register_x: 7 });
  }

}
