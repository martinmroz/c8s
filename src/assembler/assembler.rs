
use std::collections::BTreeMap;
use std::mem;

use assembler::data_range::DataRange;
use assembler::directive::*;
use assembler::instruction::*;
use assembler::parser::{Literal, Node, InstructionField};
use assembler::source_file_location::SourceFileLocation;
use assembler::u12::*;

// MARK: - Emittable

pub trait Emittable {

  /**
   @return The size of the emittable element in bytes.
   */
  fn size(&self) -> usize;

  /**
   Consuming conversion from an emittable element into an byte Vec.
   @return A Vec of bytes to be written into the output stream.
   */
  fn into_vec(self) -> Vec<u8>;

}

// MARK: - Constants

lazy_static! {
  static ref BYTES_PER_INSTRUCTION: U12 = U12::from(2);
}

// MARK: - Helper Methods

fn format_semantic_error<'a>(location: &'a SourceFileLocation, description: String) -> String {
  format!("{}: error: {}", location, description)
}

// MARK: - Pass 1: Define Labels

/**
 In a 2-pass assembler, the first pass is used to resolve the address of all labels
 declared in the assembly. This involves walking the ASL, computing the size of instructions
 and directives in the output stream and using that information to establish the label-address map.
 @param syntax_list The un-filtered syntax list.
 @return The map of labels to their corresponding addresses on success or a string describing
     the reason the first pass failed.
 */
fn define_labels<'a>(syntax_list: &Vec<Node<'a>>) -> Result<BTreeMap<&'a str, U12>, String> {
  let mut label_address_map = BTreeMap::new();
  let mut current_address = U12::zero();

  // Define all labels and process '.org' directives.
  for node in syntax_list.iter() {
    match *node {
      Node::Directive(ref data) => {
        // Validate the directive and that the arguments match the identifier.
        let directive = match Directive::from_identifier_and_parameters(data.identifier, &data.arguments) {
           Ok(result) => result,
          Err(reason) => return Err(format_semantic_error(&data.location, reason))
        };

        // Ensure the directive is not too large to emit.
        let directive_size = match directive.size().as_u12() {
          Some(size) => size,
          None => {
            let reason = format!("Directive ({}) size {} exceeds 4096 bytes.", directive, directive.size());
            return Err(format_semantic_error(&data.location, reason));
          }
        };

        // Offset the current address by the size of the directive being processed.
        current_address = match current_address.checked_add(directive_size) {
          Some(sum) => sum,
          None => {
            let reason = format!("Directive ({}) would cause the 12-bit address counter to overflow.", directive);
            return Err(format_semantic_error(&data.location, reason));
          }
        };

        // The origin directive changes the current address.
        if let Directive::Org(address) = directive {
          current_address = address;
        }
      }

      Node::Label(ref data) => {
        // Map the label to the current address and remove from the ASL.
        if label_address_map.contains_key(&data.identifier) {
          let reason = format!("Attempted re-definition of label {}.", data.identifier);
          return Err(format_semantic_error(&data.location, reason));
        } else {
          label_address_map.insert(data.identifier, current_address);
        }
      }

      Node::Instruction(ref data) => {
        // All Chip8 instructions are the same length.
        current_address = match current_address.checked_add(*BYTES_PER_INSTRUCTION) {
          Some(sum) => sum,
          None => { 
            let reason = format!("Instruction would cause address counter to overflow $FFF.");
            return Err(format_semantic_error(&data.location, reason));
          }
        };
      }
    }
  }

  // Semantic analysis for directives and labels is complete.
  Ok(label_address_map)
}

// MARK: - Pass 2: Emit Bytes

fn emit_data_ranges<'a>(syntax_list: Vec<Node<'a>>, label_address_map: &BTreeMap<&'a str, U12>) -> Result<Vec<DataRange>, String> {
  let mut data_ranges = Vec::new();
  let mut current_range = DataRange::new(U12::zero());

  for node in syntax_list {
    match node {

      Node::Directive(data) => {
        match Directive::from_identifier_and_parameters(data.identifier, &data.arguments).unwrap() {

          // Process the origin directive to build a new data range.
          Directive::Org(address) => {
            let new_range = DataRange::new(address);
            let previous_range = mem::replace(&mut current_range, new_range);
            if previous_range.len() > 0 {
              data_ranges.push(previous_range);
            }
          }

          // Emit the directive into the range.
          directive @ _ => {
            assert_eq!(current_range.append(directive.into_vec().as_slice()), true);
          }

        }
      }


      Node::Instruction(data) => {
        // Verify the semantics and append the instruction to the output buffer.
        match Instruction::from_mnemonic_and_parameters(data.mnemonic, &data.fields, label_address_map) {
          Ok(instruction) => {
            assert_eq!(current_range.append(instruction.into_vec().as_slice()), true);
          }
          Err(reason) => {
            return Err(format_semantic_error(&data.location, reason));
          }
        }
      }

      Node::Label(_) => { 
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

  println!("{:?}", label_address_map);

  Err(String::from("Mission failed."))
}
