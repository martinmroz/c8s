
#[macro_use] extern crate lazy_static;
extern crate regex;

pub mod assembler;

use assembler::scanner::Scanner;
use assembler::parser::*;
use assembler::assembler::*;

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
  file.read_to_string(&mut buffer)
      .expect(format!("Unable to read contents of file {}", path).as_str());

  // Create a scanner to process the contents.
  let scanner = Scanner::new(buffer.as_str());
  match parse(scanner) {
    Ok(asl) => {
      for node in asl.iter() {
        println!("{:?}", *node);
      }
      println!("{:?}", assemble(asl));
    }
    Err(reason) => {
      println!("Parse failed: {}", reason);
    }
  }
  
}
