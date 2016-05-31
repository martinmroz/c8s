
use std::io::prelude::*;
use std::fs::File;

#[macro_use] extern crate lazy_static;
extern crate regex;

pub mod assembler;

use assembler::scanner::Scanner;
use assembler::parser::*;
use assembler::assembler::*;
use assembler::i8hex_writer::*;

fn main() {

  let path = "/Users/martinmroz/Development/c8s/src/test.S";
  
  // Open the file specified by path.
  let mut file = match File::open(path) {
    Ok(result) => { result }
    Err(_) => { panic!("Unable to open file {}", path); }
  };
  
  let mut buffer = String::new();
  
  // Read the contents into the buffer.
  file
    .read_to_string(&mut buffer)
    .expect(format!("Unable to read contents of file {}", path).as_str());

  // Create a scanner to process the contents.
  let scanner = Scanner::new("test.S", buffer.as_str());
  let asl = match parse(scanner) {
    Ok(asl) => asl,
    Err(reason) => panic!(reason)
  };

  // Assemble the ASL into data ranges.
  let data_ranges = match assemble(asl) {
    Ok(data_ranges) => data_ranges,
    Err(reason) => panic!(reason)
  };

  // Convert the resultant data to Intel HEX (I8HEX version).
  let convertible_ranges = data_ranges.iter().map(|range| range).collect::<Vec<_>>();
  println!("{}", i8hex_representation_of_data_ranges(convertible_ranges.as_slice()));
}
