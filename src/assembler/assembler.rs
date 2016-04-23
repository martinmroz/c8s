
use std::mem;
use std::collections::BTreeMap;

use assembler::u12::*;
use assembler::opcode::Opcode;
use assembler::parser::{Literal, Node, InstructionField};

// MARK: - Result Type

#[derive(PartialEq, Debug)]
pub struct DataRange {
  /// The first address populated by the bytes in the range.
  pub start_address: usize,
  /// The bytes occupying the range.
  pub data: Vec<u8>
}

impl DataRange {
  fn new(start_address: usize) -> DataRange {
    assert!(start_address <= 0xFFF);
    DataRange {
      start_address: start_address,
      data: Vec::new()
    }
  }
}

// MARK: - Constants

const BYTES_PER_INSTRUCTION: usize = 2;
const DIRECTIVE_ORG: &'static str = "org";
const DIRECTIVE_DB : &'static str = "db";

// MARK: - Semantic Analysis

/**
 @return () in the event the arguments are valid for the directive, or a failure reason.
 */
fn validate_directive_semantics<'a>(identifier: &'a str, arguments: &Vec<Literal<'a>>) -> Result<(), String> {
  /*
   The 'org' directive is used to set the current output origin address. The directive requires
   a single numeric literal in the range $000-$FFF.
   */
  if identifier == DIRECTIVE_ORG {
    if arguments.len() != 1 {
      return Err(format!("Incorrect number of parameters ({}) for directive .org, expecting 1.", arguments.len()));
    }
    
    match arguments[0] {
      Literal::Numeric(a) if a <= 4095 => {} 
      _ => {
          return Err(format!("Directive .org requires 1 numeric literal in the range $000-$FFF."));
      }
    }

    return Ok(());
  }

  /*
   The 'db' directive is used to emit a series of bytes at the current location. 
   A list of 1 or more literals is required, either numeric or string. 
   A string literal is emitted without a null-terminator.
   */
  if identifier == DIRECTIVE_DB {
    if arguments.len() == 0 {
      return Err(format!("Incorrect number of parameters ({}) for directive .db, expecting 1 or more.", arguments.len()));
    }

    for argument in arguments {
      if let &Literal::Numeric(value) = argument {
          if value > 0xFF {
              return Err(format!("All numeric parameters to .db must be 1-byte literals (${:X} > $FF)", value));
          }
      }
    }

    return Ok(());
  }

  Err(format!("Unrecognized directive .{}", identifier))
}

/**
 @return The number of bytes the directive represents within the output stream.
 */
fn size_of_directive<'a>(identifier: &'a str, arguments: &Vec<Literal<'a>>) -> usize {
  // The origin directive is not emitted.
  if identifier == DIRECTIVE_ORG {
    return 0;
  }

  // The number of bytes emitted by .db is the sum of argument lengths.
  if identifier == DIRECTIVE_DB {
    return arguments.iter().fold(0, |acc, argument| {
      match *argument {
        Literal::Numeric(number) if number <= 0xFF => { acc + 1 }
        Literal::String(string) => { acc + string.len() }
        _ => { 
          panic!("Semantic analysis for directive .{} failed.", identifier); 
        }
      }
    });
  }

  // The first pass at semantic analysis should have caught an invalid directive.
  panic!("Semantic analysis for directive .{} failed.", identifier);
}

/**
 */
fn assemble_instruction<'a>(mnemonic: &'a str, fields: Vec<InstructionField<'a>>, label_map: &BTreeMap<&'a str, usize>) -> Result<Vec<u8>, String> {
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

      ("jp", &InstructionField::NumericLiteral(target)) => Some(Opcode::JP { target: target.as_u12().unwrap() }),

      // The 'jp' instruction may reference a label.
      ("jp", &InstructionField::Identifier(label)) => {
        match label_map.get(label) {
          Some(value) => Some(Opcode::JP { target: value.as_u12().unwrap() }),
          _ => {
            return Err(format!("Unable to resolve label {} for mnemonic {}", label, mnemonic));
          }
        }
      }

      ("call", &InstructionField::NumericLiteral(target)) => Some(Opcode::CALL { target: target.as_u12().unwrap() }),

      // The 'call' instruction may reference a label.
      ("call", &InstructionField::Identifier(label)) => {
        match label_map.get(label) {
          Some(value) => Some(Opcode::CALL { target: value.as_u12().unwrap() }),
          _ => {
            return Err(format!("Unable to resolve label {} for mnemonic {}", label, mnemonic));
          }
        }
      }

      _ => None
    }
  }

  // Mnemonics with two parameters.
  if fields.len() == 2 {
    opcode = match (mnemonic, fields.get(0).unwrap(), fields.get(1).unwrap()) {
      ("se", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
        Some(Opcode::SE_IMMEDIATE { register_x: x, value: value as u8 })
      }

      ("sne", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
        Some(Opcode::SNE_IMMEDIATE { register_x: x, value: value as u8 })
      }

      ("se", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
        Some(Opcode::SE_REGISTER { register_x: x, register_y: y })
      }

      ("sne", &InstructionField::GeneralPurposeRegister(x), &InstructionField::GeneralPurposeRegister(y)) => {
        Some(Opcode::SNE_REGISTER { register_x: x, register_y: y })
      }

      ("ld", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
        Some(Opcode::LD_IMMEDIATE { register_x: x, value: value as u8 })
      }

      ("add", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
        Some(Opcode::ADD_IMMEDIATE { register_x: x, value: value as u8 })
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
        Some(Opcode::LD_I { value: value.as_u12().unwrap() })
      }

      ("ld", &InstructionField::IndexRegister, &InstructionField::Identifier(label)) => {
        match label_map.get(label) {
          Some(value) => Some(Opcode::LD_I { value: value.as_u12().unwrap() }),
          _ => {
            return Err(format!("Unable to resolve label {} for mnemonic {}", label, mnemonic));
          }
        }
      }

      ("jp", &InstructionField::GeneralPurposeRegister(0), &InstructionField::NumericLiteral(value)) => {
        Some(Opcode::JP_V0 { value: value.as_u12().unwrap() })
      }

      ("jp", &InstructionField::GeneralPurposeRegister(0), &InstructionField::Identifier(label)) => {
        match label_map.get(label) {
          Some(value) => Some(Opcode::JP_V0 { value: value.as_u12().unwrap() }),
          _ => {
            return Err(format!("Unable to resolve label {} for mnemonic {}", label, mnemonic));
          }
        }
      }

      ("rnd", &InstructionField::GeneralPurposeRegister(x), &InstructionField::NumericLiteral(value)) => {
        Some(Opcode::RND { register_x: x, mask: value as u8 })
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
      opcode = Some(Opcode::DRW {register_x: x, register_y: y, bytes: value as u8} );
    }
  }

  // Assemble the matched instruction.
  match opcode {
    None => { Err(format!("Unable to assemble instruction {} {:?}", mnemonic, fields)) }
    Some(value) => {
      let instruction: u16 = value.as_instruction();
      let hi_8 = ((instruction & 0xFF00) >> 8) as u8;
      let lo_8 = ((instruction & 0x00FF) >> 0) as u8;
      Ok(vec![hi_8, lo_8])
    }
  }
}

// MARK: - Pass 1: Define Labels

/**
 In a 2-pass assembler, the first pass is used to resolve the address of all labels
 declared in the assembly. This involves walking the ASL, computing the size of instructions
 and directives in the output stream and using that information to establish the label-address map.
 @param syntax_list The un-filtered syntax list.
 @return A tuple of (the filtered syntax list with all labels removed,
     the map of labels to their corresponding addresses) on success or a string describing
     the reason the first pass failed.
 */
fn define_labels<'a>(syntax_list: &Vec<Node<'a>>) -> Result<BTreeMap<&'a str, usize>, String> {
  let mut label_address_map = BTreeMap::new();
  let mut current_address = 0x000;

  // Define all labels and process '.org' directives.
  for node in syntax_list.iter() {
    match *node {
      Node::Directive { identifier, ref arguments } => {
        // Validate the directive and that the arguments match the identifier.
        try!(validate_directive_semantics(identifier, arguments));

        // Adjust address by directive size.
        current_address = current_address + size_of_directive(identifier, arguments);

        // The origin directive changes the current address.
        if let (DIRECTIVE_ORG, &Literal::Numeric(address)) = (identifier, &arguments[0]) {
          current_address = address;
        }
      }

      Node::Label { identifier } => {
        // Map the label to the current address and remove from the ASL.
        if label_address_map.contains_key(&identifier) {
          return Err(format!("Attempted re-definition of label {}.", identifier));
        } else {
          label_address_map.insert(identifier, current_address);
        }
      }

      Node::Instruction { mnemonic: _, fields: _ } => {
        // All Chip8 instructions are the same length.
        current_address = current_address + BYTES_PER_INSTRUCTION;
      }
    }
  }

  // Semantic analysis for directives and labels is complete.
  Ok(label_address_map)
}

// MARK: - Pass 2: Emit Bytes

fn emit_data_ranges<'a>(syntax_list: Vec<Node<'a>>, label_address_map: &BTreeMap<&'a str, usize>) -> Result<Vec<DataRange>, String> {
  let mut data_ranges = Vec::new();
  let mut current_range = DataRange::new(0x000);

  for node in syntax_list {
    match node {
      Node::Directive { identifier, arguments } => {
        match identifier {
          DIRECTIVE_ORG => {
            // Push the current data range into the list of ranges and start a new one at the 'org' point.
            if let &Literal::Numeric(address) = arguments.get(0).unwrap() {
              let new_range = DataRange::new(address);
              let previous_range = mem::replace(&mut current_range, new_range);
              data_ranges.push(previous_range);
            } else {
              panic!("Internal assembler error: Invalid directive {} not removed prior to emit_data_ranges().", identifier);
            }
          }
          
          DIRECTIVE_DB  => {
            // Emit all the literal arguments directly to the data range.
            for literal in arguments {
              match literal {
                Literal::Numeric(numeric) => {
                  assert!(numeric <= 0xFF);
                  current_range.data.push(numeric as u8);
                }
                Literal::String(string) => {
                  current_range.data.extend_from_slice(string.as_bytes());
                }
              }
            }
          }

          _ => {
            panic!("Internal assembler error: Invalid directive {} not removed prior to emit_data_ranges().", identifier);
          }
        }
      }

      Node::Instruction { mnemonic, fields } => {
        // Verify the semantics and convert the instruction into a byte array.
        let bytes = try!(assemble_instruction(mnemonic, fields, label_address_map));
        current_range.data.extend(bytes);
      }

      Node::Label { identifier: _ } => {
        // Processed in Pass 1.
      }
    }
  }

  // Push the last range into the list.
  data_ranges.push(current_range);
  Ok(data_ranges)
}

// MARK: - Public API

/**
 Analyze the ASL for the assembly and convert it into an output byte stream.
 */
pub fn assemble<'a>(syntax_list: Vec<Node<'a>>) -> Result<Vec<DataRange>, String> {
    let label_address_map = try!(define_labels(&syntax_list));
    emit_data_ranges(syntax_list, &label_address_map)
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use super::*;
  use super::{validate_directive_semantics, size_of_directive};
  use super::define_labels;

  use assembler::parser::*;

  // MARK: - Pass 1 Tests

  #[test]
  fn test_validate_directive_semantics_for_org() {
    let mut params = vec![];
    assert_eq!(validate_directive_semantics("org", &params), Err("Incorrect number of parameters (0) for directive .org, expecting 1.".to_string()));
    params = vec![Literal::Numeric(1), Literal::Numeric(3)];
    assert_eq!(validate_directive_semantics("org", &params), Err("Incorrect number of parameters (2) for directive .org, expecting 1.".to_string()));
    params = vec![Literal::Numeric(0x1000)];
    assert_eq!(validate_directive_semantics("org", &params), Err("Directive .org requires 1 numeric literal in the range $000-$FFF.".to_string()));
    params = vec![Literal::String("TEST_STRING")];
    assert_eq!(validate_directive_semantics("org", &params), Err("Directive .org requires 1 numeric literal in the range $000-$FFF.".to_string()));
    params = vec![Literal::Numeric(0xFFF)];
    assert_eq!(validate_directive_semantics("org", &params), Ok(()));
    params = vec![Literal::Numeric(0x000)];
    assert_eq!(validate_directive_semantics("org", &params), Ok(()));
  }

  #[test]
  fn test_validate_directive_semantics_for_db() {
    let mut params = vec![];
    assert_eq!(validate_directive_semantics("db", &params), Err("Incorrect number of parameters (0) for directive .db, expecting 1 or more.".to_string()));
    params = vec![Literal::Numeric(0x100)];
    assert_eq!(validate_directive_semantics("db", &params), Err("All numeric parameters to .db must be 1-byte literals ($100 > $FF)".to_string()));
    params = vec![Literal::String("TEST_STRING")];
    assert_eq!(validate_directive_semantics("db", &params), Ok(()));
    params = vec![Literal::Numeric(0xFF)];
    assert_eq!(validate_directive_semantics("db", &params), Ok(()));
    params = vec![Literal::Numeric(0x00)];
    assert_eq!(validate_directive_semantics("db", &params), Ok(()));
    params = vec![Literal::String("TEST_STRING"), Literal::Numeric(0x00)];
    assert_eq!(validate_directive_semantics("db", &params), Ok(()));
  }

  #[test]
  fn test_validate_directive_semantics_for_invalid_directive() {
    let mut params = vec![];
    assert_eq!(validate_directive_semantics("dw", &params), Err("Unrecognized directive .dw".to_string()));
    params = vec![Literal::Numeric(0x100)];
    assert_eq!(validate_directive_semantics("dw", &params), Err("Unrecognized directive .dw".to_string()));
  }

  #[test]
  fn test_size_of_directive_for_org() {
    assert_eq!(size_of_directive("org", &vec![]), 0);
    assert_eq!(size_of_directive("org", &vec![Literal::Numeric(0x100)]), 0);
    assert_eq!(size_of_directive("org", &vec![Literal::Numeric(0x100), Literal::String("TEST_STRING")]), 0);
  }

  #[test]
  fn test_size_of_directive_for_db() {
    assert_eq!(size_of_directive("db", &vec![]), 0);
    assert_eq!(size_of_directive("db", &vec![Literal::Numeric(0xFF)]), 1);
    assert_eq!(size_of_directive("db", &vec![Literal::String("TEST_STRING")]), 11);
    assert_eq!(size_of_directive("db", &vec![Literal::String("TEST_STRING"), Literal::Numeric(0x00)]), 12);
  }

  #[test]
  #[should_panic]
  fn test_size_of_directive_panics_for_invalid_directive() {
    size_of_directive("dw", &vec![]);
  }

  #[test]
  fn test_define_labels() {
    let program = vec![
      Node::Directive   { identifier: "org", arguments: vec![Literal::Numeric(0x100)] },
      Node::Label       { identifier: "label1" },
      Node::Directive   { identifier: "db",  arguments: vec![Literal::Numeric(0xFF)]  },
      Node::Label       { identifier: "label2" },
      Node::Instruction {   mnemonic: "trap",   fields: vec![] },
      Node::Label       { identifier: "label3" }
    ];

    let result = define_labels(&program);

    // Assert that semantic analysis passed.
    if let Err(reason) = result {
      assert!(false, "Unexpected failure in assembler pass 1: {}", reason);
      return;
    }

    // Verify that the 
    if let Ok(label_map) = result {
      assert_eq!(label_map.len(), 3);
      assert_eq!(label_map.get("label1").unwrap(), &0x100);
      assert_eq!(label_map.get("label2").unwrap(), &0x101);
      assert_eq!(label_map.get("label3").unwrap(), &0x103);
    }
  }

  #[test]
  fn test_define_labels_does_semantic_analysis_on_org() {
    let program = vec![
      Node::Directive { identifier: "org", arguments: vec![] },
    ];

    let result = define_labels(&program);
    assert_eq!(result, Err("Incorrect number of parameters (0) for directive .org, expecting 1.".to_string()));
  }

  #[test]
  fn test_define_labels_does_semantic_analysis_on_db() {
    let program = vec![
      Node::Directive { identifier: "db", arguments: vec![] },
    ];

    let result = define_labels(&program);
    assert_eq!(result, Err("Incorrect number of parameters (0) for directive .db, expecting 1 or more.".to_string()));
  }

  #[test]
  fn test_define_labels_fails_on_redefinition() {
    let program = vec![
      Node::Label { identifier: "L1" },
      Node::Label { identifier: "L1" }
    ];

    let result = define_labels(&program);
    assert_eq!(result, Err("Attempted re-definition of label L1.".to_string()));
  }

  // MARK: - Pass 2 Tests



}
