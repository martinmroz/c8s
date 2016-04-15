
use std::collections::BTreeMap;

use assembler::parser::{Literal, Node, InstructionField};

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
fn define_and_filter_labels<'a>(syntax_list: Vec<Node<'a>>) -> Result<(Vec<Node<'a>>, BTreeMap<&'a str, usize>), String> {
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
                }

                label_address_map.insert(identifier, current_address);
            }

            Node::Instruction { mnemonic: _, fields: _ } => {
                // All Chip8 instructions are the same length.
                current_address = current_address + BYTES_PER_INSTRUCTION;
            }
        }
    }

    // Remove any label nodes from the ASL as they have now been resolved.
    let filtered_asl = syntax_list.into_iter().filter(|node| {
        match *node {
            Node::Label { identifier: _ } => false,
            Node::Directive { identifier: _, arguments: _ } => true,
            Node::Instruction { mnemonic: _, fields: _ } => true
        }
    }).collect();

    // Semantic analysis for directives and labels is complete.
    Ok((filtered_asl, label_address_map))
}

// MARK: - Pass 2: Emit Bytes

fn emit_data_ranges<'a>(syntax_list: Vec<Node<'a>>, BTreeMap<&'a str, usize>) -> Result<(), String> {
  Ok(())
}

// MARK: - Public API

/**
 Analyze the ASL for the assembly and convert it into an output byte stream.
 */
pub fn assemble<'a>(syntax_list: Vec<Node<'a>>) -> Result<(), String> {
    let _ = try!(define_and_filter_labels(syntax_list));
    Ok(())
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use super::*;
  use super::{validate_directive_semantics, size_of_directive};
  use super::define_and_filter_labels;

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
  fn test_define_and_filter_labels() {
    let program = vec![
      Node::Directive   { identifier: "org", arguments: vec![Literal::Numeric(0x100)] },
      Node::Label       { identifier: "label1" },
      Node::Directive   { identifier: "db",  arguments: vec![Literal::Numeric(0xFF)]  },
      Node::Label       { identifier: "label2" },
      Node::Instruction {   mnemonic: "trap",   fields: vec![] },
      Node::Label       { identifier: "label3" }
    ];

    let result = define_and_filter_labels(program);

    // Assert that semantic analysis passed.
    if let Err(reason) = result {
      assert!(false, "Unexpected failure in assembler pass 1: {}", reason);
      return;
    }

    // Verify that the 
    if let Ok((filtered_asl, label_map)) = result {
      assert_eq!(label_map.len(), 3);
      assert_eq!(label_map.get("label1").unwrap(), &0x100);
      assert_eq!(label_map.get("label2").unwrap(), &0x101);
      assert_eq!(label_map.get("label3").unwrap(), &0x103);
      assert_eq!(filtered_asl.len(), 3);
      assert_eq!(filtered_asl.get(0).unwrap(), &Node::Directive { identifier: "org", arguments: vec![Literal::Numeric(0x100)] });
      assert_eq!(filtered_asl.get(1).unwrap(), &Node::Directive { identifier: "db",  arguments: vec![Literal::Numeric(0xFF)] });
      assert_eq!(filtered_asl.get(2).unwrap(), &Node::Instruction { mnemonic: "trap", fields: vec![] });
    }
  }

  #[test]
  fn test_define_and_filter_labels_does_semantic_analysis_on_org() {
    let program = vec![
      Node::Directive { identifier: "org", arguments: vec![] },
    ];

    let result = define_and_filter_labels(program);
    assert_eq!(result, Err("Incorrect number of parameters (0) for directive .org, expecting 1.".to_string()));
  }

  #[test]
  fn test_define_and_filter_labels_does_semantic_analysis_on_db() {
    let program = vec![
      Node::Directive { identifier: "db", arguments: vec![] },
    ];

    let result = define_and_filter_labels(program);
    assert_eq!(result, Err("Incorrect number of parameters (0) for directive .db, expecting 1 or more.".to_string()));
  }

  #[test]
  fn test_define_and_filter_labels_fails_on_redefinition() {
    let program = vec![
      Node::Label { identifier: "L1" },
      Node::Label { identifier: "L1" }
    ];

    let result = define_and_filter_labels(program);
    assert_eq!(result, Err("Attempted re-definition of label L1.".to_string()));
  }

  // MARK: - Pass 2 Tests



}
