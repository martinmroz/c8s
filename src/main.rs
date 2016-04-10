
#[macro_use] extern crate lazy_static;
extern crate regex;

pub mod assembler;

use assembler::scanner::Scanner;
use assembler::parser::*;

use std::io::prelude::*;
use std::fs::File;

fn main() {
  
  let path = "/Users/martinmroz/Development/c8s/src/test.S";
  
  // Open the file specified by path.
  let mut file = match File::open(path) {
    Ok(result) => { result }
    Err(_) => { panic!("Unable to open file {}", path); }
  };
  
  let mut buffer = String::new();
  
  // Read the contents into the buffer.
  match file.read_to_string(&mut buffer) {
    Ok(_) => {}
    Err(_) => { panic!("Unable to read contents of file {}", path); }
  };
  
  // Create a scanner to process the contents.
  let scanner = Scanner::new(buffer.as_str());
  for node in parse(scanner).ok().unwrap() {
    println!("{:?}", node);
  }
  
}
