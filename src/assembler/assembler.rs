
use std::collections::BTreeMap;
use std::mem;

use assembler::data_range::DataRange;
use assembler::directive::*;
use assembler::instruction::*;
use assembler::opcode::Opcode;
use assembler::parser::{Literal, Node, InstructionField};
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
  fn into_slice(self) -> Vec<u8>;

}

// MARK: - Constants

const BYTES_PER_INSTRUCTION: u8 = 2;

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
        let directive = try!(Directive::from_identifier_and_parameters(data.identifier, &data.arguments));

        // Ensure the directive is not too large to emit.
        let directive_size = match directive.size().as_u12() {
                None => { return Err(format!("Directive too large.")); }
          Some(size) => { size }
        };

        // Offset the current address by the size of the directive being processed.
        current_address = match current_address.checked_add(directive_size) {
                None => { return Err(format!("Directive would cause address counter to overflow.")); }
           Some(sum) => { sum }
        };

        // The origin directive changes the current address.
        if let Directive::Org(address) = directive {
          current_address = address;
        }
      }

      Node::Label(ref data) => {
        // Map the label to the current address and remove from the ASL.
        if label_address_map.contains_key(&data.identifier) {
          return Err(format!("Attempted re-definition of label {}.", data.identifier));
        } else {
          label_address_map.insert(data.identifier, current_address);
        }
      }

      Node::Instruction(_) => {
        // All Chip8 instructions are the same length.
        current_address = match current_address.checked_add(U12::from(BYTES_PER_INSTRUCTION)) {
                None => { return Err(format!("Instruction would cause address counter to overflow.")); }
           Some(sum) => { sum }
        };
      }
    }
  }

  // Semantic analysis for directives and labels is complete.
  Ok(label_address_map)
}

// MARK: - Public API

/**
 Analyze the ASL for the assembly and convert it into an output byte stream.
 */
pub fn assemble<'a>(syntax_list: Vec<Node<'a>>) -> Result<Vec<DataRange>, String> {
    Err(String::from("Mission failed."))
}
