
use std::collections::BTreeMap;

use assembler::assembler::Emittable;
use assembler::opcode::Opcode;
use assembler::parser::{Literal, InstructionField};
use assembler::u12::*;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct Instruction(Opcode);

/**
 Resolves the label from the label map.
 @param label The name of the label (case-sensitive).
 @param label_map The mapping of labels to their resolved addresses.
 @return The address from the label map if defined, or an error describing the failure.
 */
fn resolve_label_with_map<'a>(label: &'a str, label_map: &BTreeMap<&'a str, U12>) -> Result<U12, String> {
  match label_map.get(label) {
    Some(value) => Ok(*value),
           None => Err(format!("Unable to resolve address of label {}", label))
  }
}

/**
 Converts a 12-bit (unsigned) literal value to 8-bits if possible to do so without data loss.
 @param literal 12-bit literal.
 @return A result consisting of the low 8 bits of the 12-bit literal or an explanation as to why conversion failed.
 */
fn numeric_literal_to_8_bit_field(literal: U12) -> Result<u8, String> {
  match literal.as_u8() {
    Some(value) => Ok(value),
           None => Err(format!("Found 12-bit numeric literal ${:X}, expecting 8-bit value.", literal.as_usize()))
  }
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
	pub fn from_mnemonic_and_parameters(mnemonic: &'a str, fields: Vec<InstructionField<'a>>, label_map: &BTreeMap<&'a str, U12>) -> Result<Self, String> {
		let mut opcode: Option<Opcode> = None;

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

        ("jp", &InstructionField::NumericLiteral(target)) => Some(Opcode::JP { target: target }),

        // The 'jp' mnemonic may reference a label.
        ("jp", &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::JP { target: address })
        }

        ("call", &InstructionField::NumericLiteral(target)) => Some(Opcode::CALL { target: target }),

        // The 'call' mnemonic may reference a label.
        ("call", &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::CALL { target: address })
        }

        _ => None
      }
    }

    // Mnemonics with two parameters.
    if fields.len() == 2 {
      opcode = match (mnemonic, fields.get(0).unwrap(), fields.get(1).unwrap()) {
        ("se", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::SE_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(value)) })
        }

        ("sne", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::SNE_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(value)) })
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

        ("add", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::ADD_IMMEDIATE { register_x: x, value: try!(numeric_literal_to_8_bit_field(value)) })
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

        ("shr", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::SHR { register_x: x, register_y: y })
        }

        ("subn", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::SUBN { register_x: x, register_y: y })
        }

        ("shl", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
          Some(Opcode::SHL { register_x: x, register_y: y })
        }

        ("ld", &InstructionField::IndexRegister, &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::LD_I { value: value })
        }

        // The mnemonic 'ld i' can take a label parameter.
        ("ld", &InstructionField::IndexRegister, &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::LD_I { value: address })
        }

        ("jp", &InstructionField::GeneralPurposeRegister(0), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::JP_V0 { value: value })
        }

        // The mnemonic 'jp v0' can take a label parameter.
        ("jp", &InstructionField::GeneralPurposeRegister(0), &InstructionField::Identifier(label)) => {
          let address = try!(resolve_label_with_map(label, label_map));
          Some(Opcode::JP_V0 { value: address })
        }

        ("rnd", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
          Some(Opcode::RND { register_x: x, mask: try!(numeric_literal_to_8_bit_field(value)) })
        }

        ("se", &InstructionField::GeneralPurposeRegister(x), &InstructionField::KeypadRegister) => {
          Some(Opcode::SE_K { register_x: x })
        }

        ("sne", &InstructionField::GeneralPurposeRegister(x), &InstructionField::KeypadRegister) => {
          Some(Opcode::SNE_K { register_x: x })
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

        ("sprite", &InstructionField::IndexRegister, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SPRITE_I { register_x: x })
        }

        ("bcd", &InstructionField::IndexRegisterIndirect, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::BCD_I { register_x: x })
        }

        ("save", &InstructionField::IndexRegisterIndirect, &InstructionField::GeneralPurposeRegister(x)) => {
          Some(Opcode::SAVE_I { register_x: x })
        }

        ("restore", &InstructionField::IndexRegisterIndirect, &InstructionField::GeneralPurposeRegister(x)) => {
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
        opcode = Some(Opcode::DRW {register_x: x, register_y: y, bytes: try!(numeric_literal_to_8_bit_field(value)) } );
      }
    }

    // Assemble the matched instruction.
    return match opcode {
      Some(value) => Ok(Instruction(value)),
      None => {
        match fields.len() {
          0 => Err(format!("No matching format for instruction: {}", mnemonic)),
          _ => {
            let field_strings = fields.iter().map(|field| format!("{}", field));
            let field_description = field_strings.collect::<Vec<_>>().connect(",");
            Err(format!("No matching format for instruction: {} {}", mnemonic, field_description))
          }
        }
      }
    };
	}
}

impl Emittable for Instruction {

  /**
   @return The size of the instruction in the output stream in bytes (always 2).
   */
  fn size(&self) -> usize { 2 }

  /**
   Consuming conversion from a directive element into the byte representation in the output stream.
   @return A Vec of bytes to be written into the output stream for the directive.
   */
  fn into_slice(self) -> Vec<u8> {
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

  use std::collections::BTreeMap;

  use assembler::assembler::Emittable;
  use assembler::opcode::Opcode;
  use assembler::parser::{InstructionField, Literal};
  use assembler::u12;
  use assembler::u12::*;

  use super::*;

  #[test]
  pub fn test_nop() {
    let empty_map = BTreeMap::new();
    let nop = Instruction::from_mnemonic_and_parameters("nop", vec![], &empty_map).unwrap();
    assert_eq!(nop.size(), 2);
    assert_eq!(nop.0, Opcode::NOP);
  }

  #[test]
  pub fn test_cls() {
    let empty_map = BTreeMap::new();
    let cls = Instruction::from_mnemonic_and_parameters("cls", vec![], &empty_map).unwrap();
    assert_eq!(cls.size(), 2);
    assert_eq!(cls.0, Opcode::CLS);
  }

  #[test]
  pub fn test_ret() {
    let empty_map = BTreeMap::new();
    let ret = Instruction::from_mnemonic_and_parameters("ret", vec![], &empty_map).unwrap();
    assert_eq!(ret.size(), 2);
    assert_eq!(ret.0, Opcode::RET);
  }

  #[test]
  pub fn test_trapret() {
    let empty_map = BTreeMap::new();
    let trapret = Instruction::from_mnemonic_and_parameters("trapret", vec![], &empty_map).unwrap();
    assert_eq!(trapret.size(), 2);
    assert_eq!(trapret.0, Opcode::TRAPRET);
  }

  #[test]
  pub fn test_trap() {
    let empty_map = BTreeMap::new();
    let trap = Instruction::from_mnemonic_and_parameters("trap", vec![], &empty_map).unwrap();
    assert_eq!(trap.size(), 2);
    assert_eq!(trap.0, Opcode::TRAP);
  }

  #[test]
  pub fn test_jp() {
    let empty_map = BTreeMap::new();
    let mut defined_map = BTreeMap::new();
    defined_map.insert("TEST_LABEL", u12::MAX);

    // Failure Mode 1: No Parameters.
    let invalid_jp = Instruction::from_mnemonic_and_parameters("jp", vec![], &empty_map);
    assert_eq!(invalid_jp.is_err(), true);
    assert_eq!(invalid_jp.unwrap_err(), String::from("No matching format for instruction: jp"));

    // Failure Mode 2: Undefined Label.
    let label_field = InstructionField::Identifier("TEST_LABEL");
    let invalid_jp_nl = Instruction::from_mnemonic_and_parameters("jp", vec![label_field.clone()], &empty_map);
    assert_eq!(invalid_jp_nl.is_err(), true);
    assert_eq!(invalid_jp_nl.unwrap_err(), String::from("Unable to resolve address of label TEST_LABEL"));

    // Success 1: Literal 12-bit Numeric.
    let literal_field = InstructionField::NumericLiteral(u12::MAX);
    let jp_literal = Instruction::from_mnemonic_and_parameters("jp", vec![literal_field], &empty_map).unwrap();
    assert_eq!(jp_literal.size(), 2);
    assert_eq!(jp_literal.0, Opcode::JP { target: u12::MAX });

    // Success 2: Defined 12-bit Label.
    let jp_label = Instruction::from_mnemonic_and_parameters("jp", vec![label_field], &defined_map).unwrap();
    assert_eq!(jp_label.size(), 2);
    assert_eq!(jp_label.0, Opcode::JP { target: u12::MAX });
  }

  #[test]
  pub fn test_call() {
    let empty_map = BTreeMap::new();
    let mut defined_map = BTreeMap::new();
    defined_map.insert("TEST_LABEL", u12::MAX);

    // Failure Mode 1: No Parameters.
    let invalid_call = Instruction::from_mnemonic_and_parameters("call", vec![], &empty_map);
    assert_eq!(invalid_call.is_err(), true);
    assert_eq!(invalid_call.unwrap_err(), String::from("No matching format for instruction: call"));

    // Failure Mode 2: Undefined Label.
    let label_field = InstructionField::Identifier("TEST_LABEL");
    let invalid_call_nl = Instruction::from_mnemonic_and_parameters("call", vec![label_field.clone()], &empty_map);
    assert_eq!(invalid_call_nl.is_err(), true);
    assert_eq!(invalid_call_nl.unwrap_err(), String::from("Unable to resolve address of label TEST_LABEL"));

    // Success 1: Literal 12-bit Numeric.
    let literal_field = InstructionField::NumericLiteral(u12::MAX);
    let call_literal = Instruction::from_mnemonic_and_parameters("call", vec![literal_field], &empty_map).unwrap();
    assert_eq!(call_literal.size(), 2);
    assert_eq!(call_literal.0, Opcode::CALL { target: u12::MAX });

    // Success 2: Defined 12-bit Label.
    let call_label = Instruction::from_mnemonic_and_parameters("call", vec![label_field], &defined_map).unwrap();
    assert_eq!(call_label.size(), 2);
    assert_eq!(call_label.0, Opcode::CALL { target: u12::MAX });
  }

  #[test]
  pub fn test_se_immediate_and_register() {
    let empty_map = BTreeMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);

    // Skip Next If vX == vY.
    let se_rr = Instruction::from_mnemonic_and_parameters("se", vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(se_rr.size(), 2);
    assert_eq!(se_rr.0, Opcode::SE_REGISTER { register_x: 1, register_y: 2 });

    // Skip next if vX == nn.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xFF));
    let se_imm = Instruction::from_mnemonic_and_parameters("se", vec![register_field_1, immediate_field], &empty_map).unwrap();
    assert_eq!(se_imm.size(), 2);
    assert_eq!(se_imm.0, Opcode::SE_IMMEDIATE { register_x: 1, value: 0xFF });

    // Immediate fields for se vX, $nn is limited to 8 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(0x100.as_u12().unwrap());
    let invalid_se_imm = Instruction::from_mnemonic_and_parameters("se", vec![register_field_1, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_se_imm.is_err(), true);
    assert_eq!(invalid_se_imm.unwrap_err(), String::from("Found 12-bit numeric literal $100, expecting 8-bit value."));
  }

  #[test]
  pub fn test_sne_immediate_and_register() {
    let empty_map = BTreeMap::new();
    let register_field_1 = InstructionField::GeneralPurposeRegister(1);
    let register_field_2 = InstructionField::GeneralPurposeRegister(2);

    // Skip Next If vX != vY.
    let sne_rr = Instruction::from_mnemonic_and_parameters("sne", vec![register_field_1, register_field_2], &empty_map).unwrap();
    assert_eq!(sne_rr.size(), 2);
    assert_eq!(sne_rr.0, Opcode::SNE_REGISTER { register_x: 1, register_y: 2 });

    // Skip next if vX != nn.
    let immediate_field = InstructionField::NumericLiteral(U12::from(0xFF));
    let sne_imm = Instruction::from_mnemonic_and_parameters("sne", vec![register_field_1, immediate_field], &empty_map).unwrap();
    assert_eq!(sne_imm.size(), 2);
    assert_eq!(sne_imm.0, Opcode::SNE_IMMEDIATE { register_x: 1, value: 0xFF });

    // Immediate fields for sne vX, $nn is limited to 8 bits.
    let immediate_field_invalid = InstructionField::NumericLiteral(0x100.as_u12().unwrap());
    let invalid_sne_imm = Instruction::from_mnemonic_and_parameters("sne", vec![register_field_1, immediate_field_invalid], &empty_map);
    assert_eq!(invalid_sne_imm.is_err(), true);
    assert_eq!(invalid_sne_imm.unwrap_err(), String::from("Found 12-bit numeric literal $100, expecting 8-bit value."));
  }

}
