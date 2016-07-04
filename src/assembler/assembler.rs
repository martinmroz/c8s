//
// Copyright 2016 The c8s Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>.
// All files in the project carrying such notice may not be copied, modified, or 
// distributed except according to those terms.
//

use std::collections::BTreeMap;
use std::error;
use std::fmt;
use std::mem;

use twelve_bit::u12::*;

use assembler::data_range::DataRange;
use assembler::directive;
use assembler::directive::Directive;
use assembler::instruction;
use assembler::instruction::Instruction;
use assembler::parser::Node;
use assembler::source_file_location::SourceFileLocation;

// MARK: - Constants

lazy_static! {
  static ref BYTES_PER_INSTRUCTION: U12 = u12![2];
}

// MARK: - Semantic Error

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SemanticError<'a> {
  /// Unable to process an invalid directive.
  DirectiveInvalid(SourceFileLocation<'a>, directive::Error),
  /// The directive processed exceeds the size limit of 4096 bytes.
  DirectiveTooLarge(SourceFileLocation<'a>, Directive),
  /// The directive at the specified location would cause the address counter to wrap.
  DirectiveWouldOverflow(SourceFileLocation<'a>, Directive),
  /// The instruction at the specified location would cause the addresss counter to wrap.
  InstructionWouldOverlow(SourceFileLocation<'a>),
  /// The label specified has already been defined.
  RedefinitionOfLabel(SourceFileLocation<'a>, String),
  /// Unable to assemble the specified instruction.
  AssemblyFailed(SourceFileLocation<'a>, instruction::Error)
}

impl<'a> error::Error for SemanticError<'a> {

  /// Returns a string slice with a general description of a semantic error.
  /// No specific information is contained. To obtain a printable representation,
  /// use the `fmt::Display` attribute.
  fn description(&self) -> &str {
    match self {
      &SemanticError::DirectiveInvalid(_,_)       => "Directive invalid",
      &SemanticError::DirectiveTooLarge(_,_)      => "Directive size exceeds 4096 bytes",
      &SemanticError::DirectiveWouldOverflow(_,_) => "Directive would cause the 12-bit address counter to overflow",
      &SemanticError::InstructionWouldOverlow(_)  => "Instruction would cause address counter to overflow $FFF",
      &SemanticError::RedefinitionOfLabel(_,_)    => "Attempted re-definition of label",
      &SemanticError::AssemblyFailed(_,_)         => "Unable to assemble instruction",
    }
  }

  /// Returns the underlying error which caused the receiver.
  fn cause(&self) -> Option<&error::Error> {
    match self {
      &SemanticError::DirectiveInvalid(_, ref error) => 
        Some(error),
      &SemanticError::AssemblyFailed(_, ref error) => 
        Some(error),
      _ =>
        None,
    }
  }

}

impl<'a> fmt::Display for SemanticError<'a> {
  /// Formats the receiver for display purposes. Incorporates specific information
  /// relating to this particular error instance where applicable.
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &SemanticError::DirectiveInvalid(ref loc, ref reason) =>
        write!(f, "{}: error: {}", loc, reason),
      &SemanticError::DirectiveTooLarge(ref loc, ref directive) =>
        write!(f, "{}: error: Directive ({}) size {} exceeds 4096 bytes.", loc, directive, directive.size()),
      &SemanticError::DirectiveWouldOverflow(ref loc, ref directive) =>
        write!(f, "{}: error: Directive ({}) would cause the 12-bit address counter to overflow.", loc, directive),
      &SemanticError::RedefinitionOfLabel(ref loc, ref label) =>
        write!(f, "{}: error: Attempted re-definition of label '{}'.", loc, label),
      &SemanticError::InstructionWouldOverlow(ref loc) =>
        write!(f, "{}: error: {}", loc, (self as &error::Error).description()),
      &SemanticError::AssemblyFailed(ref loc, ref reason) =>
        write!(f, "{}: error: {}", loc, reason),
    }
  }
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
fn define_labels<'a>(syntax_list: &Vec<Node<'a>>) -> Result<BTreeMap<&'a str, U12>, SemanticError<'a>> {
  let mut label_address_map = BTreeMap::new();
  let mut current_address = u12![0];

  // Define all labels and process '.org' directives.
  for node in syntax_list.iter() {
    match *node {
      Node::Directive(ref data) => {
        // Validate the directive and that the arguments match the identifier.
        let directive = try!(
          Directive::from_identifier_and_parameters(data.identifier, &data.arguments)
            .map_err(|reason| {
              SemanticError::DirectiveInvalid(data.location.clone(), reason)
            })
        );

        // Ensure the directive is not too large to emit.
        let directive_size: U12 = try!(
          directive
            .size()
            .failable_into()
            .ok_or(SemanticError::DirectiveTooLarge(data.location.clone(), directive.clone()))
        );

        // Offset the current address by the size of the directive being processed.
        current_address = try!(
          current_address
            .checked_add(directive_size)
            .ok_or(SemanticError::DirectiveWouldOverflow(data.location.clone(), directive.clone()))
        );

        // The origin directive changes the current address.
        if let Directive::Org(address) = directive {
          current_address = address;
        }
      }

      Node::Label(ref data) => {
        // Map the label to the current address and remove from the ASL.
        if label_address_map.contains_key(&data.identifier) {
          return Err(SemanticError::RedefinitionOfLabel(data.location.clone(), String::from(data.identifier)));
        } else {
          label_address_map.insert(data.identifier, current_address);
        }
      }

      Node::Instruction(ref data) => {
        // All Chip8 instructions are the same length.
        current_address = try!(
          current_address
            .checked_add(*BYTES_PER_INSTRUCTION)
            .ok_or(SemanticError::InstructionWouldOverlow(data.location.clone()))
        );
      }
    }
  }

  // Semantic analysis for directives and labels is complete.
  Ok(label_address_map)
}

// MARK: - Pass 2: Emit Bytes

fn emit_data_ranges<'a>(syntax_list: Vec<Node<'a>>, label_address_map: &BTreeMap<&'a str, U12>) -> Result<Vec<DataRange>, SemanticError<'a>> {
  let mut data_ranges = Vec::new();
  let mut current_range = DataRange::new(u12![0]);

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
            let bytes = directive.into_vec();
            assert!(current_range.append(&bytes));
          }

        }
      }

      Node::Instruction(data) => {
        // Verify the semantics and append the instruction to the output buffer.
        let instruction = try!(
          Instruction::from_mnemonic_and_parameters(data.mnemonic, &data.fields, label_address_map)
            .map_err(|reason| {
              SemanticError::AssemblyFailed(data.location.clone(), reason)
            })
        );

        let bytes = instruction.into_vec();
        assert!(current_range.append(&bytes));
      }

      Node::Label(_) => { 
        // Processed in Pass 1. 
      }

    }
  }

  // Push the last range into the list.
  if current_range.len() > 0 {
    data_ranges.push(current_range);
  }

  Ok(data_ranges)
}

// MARK: - Public API

/**
 Analyze the ASL for the assembly and convert it into an output byte stream.
 */
pub fn assemble<'a>(syntax_list: Vec<Node<'a>>) -> Result<Vec<DataRange>, SemanticError<'a>> {
  let label_address_map = try!(define_labels(&syntax_list));
  emit_data_ranges(syntax_list, &label_address_map)
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use twelve_bit::u12::*;
  
  use assembler::parser::*;
  use assembler::source_file_location::SourceFileLocation;

  use super::define_labels;

  // MARK: - Helpers

  /// Creates a Label type node with associated data at location "-:seq:1-name.len()".
  fn make_label_node<'a>(seq: usize, name: &'a str) -> Node<'a> {
    Node::Label (
      LabelData {
        location: SourceFileLocation::new("-", seq, 1, name.len()),
        identifier: name
      }
    )
  }

  /// Creates a Directive type node with associated data at location "-:seq:2-name.len()+1".
  fn make_directive_node<'a>(seq: usize, name: &'a str, args: Vec<Literal<'a>>) -> Node<'a> {
    Node::Directive (
      DirectiveData {
        location: SourceFileLocation::new("-", seq, 2, name.len()),
        identifier: name,
        arguments: args
      }
    )
  }

  /// Creates an Instruction type node with associated data at location "-:seq:1-name.len()".
  fn make_instruction_node<'a>(seq: usize, name: &'a str, fields: Vec<InstructionField<'a>>) -> Node<'a> {
    Node::Instruction (
      InstructionData {
        location: SourceFileLocation::new("-", seq, 1, name.len()),
        mnemonic: name,
        fields: fields
      }
    )
  }

  // MARK: - Pass 1 Tests

  #[test]
  fn test_define_labels() {
    let program = vec![
      make_directive_node   (1, "org", vec![Literal::Numeric(0x100)]),
      make_label_node       (2, "label1"),
      make_directive_node   (3, "db", vec![Literal::Numeric(0xFF)]),
      make_label_node       (4, "label2"),
      make_instruction_node (5, "trap", vec![]),
      make_label_node       (6, "label3"),
    ];

    let result = define_labels(&program);

    // Assert that semantic analysis passed.
    if let Err(reason) = result {
      assert!(false, "Unexpected failure in assembler pass 1: {}", reason);
      return;
    }

    // Verify that the labels are defined as expected.
    let label_map = result.unwrap();
    assert_eq!(label_map.len(), 3);
    assert_eq!(label_map.get("label1").unwrap(), &(u12![0x100]));
    assert_eq!(label_map.get("label2").unwrap(), &(u12![0x101]));
    assert_eq!(label_map.get("label3").unwrap(), &(u12![0x103]));
  }

  #[test]
  fn test_define_labels_does_semantic_analysis_on_org() {
    let program = vec![
      make_directive_node(1, "org", vec![])
    ];

    let result = define_labels(&program);
    assert_eq!(format!("{}", result.unwrap_err()), "-:1:2-4: error: Incorrect number of parameters (0) for directive .org, expecting 1.");
  }

  #[test]
  fn test_define_labels_does_semantic_analysis_on_db() {
    let program = vec![
      make_directive_node(1, "db", vec![])
    ];

    let result = define_labels(&program);
    assert_eq!(format!("{}", result.unwrap_err()), "-:1:2-3: error: Incorrect number of parameters (0) for directive .db, expecting 1 or more.");
  }

  #[test]
  fn test_define_labels_fails_on_redefinition() {
    let program = vec![
      make_label_node(1, "L1"),
      make_label_node(2, "L1"),
    ];

    let result = define_labels(&program);
    assert_eq!(format!("{}", result.unwrap_err()), "-:2:1-2: error: Attempted re-definition of label 'L1'.");
  }

}
