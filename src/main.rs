
#[macro_use]
extern crate lazy_static;

extern crate getopts;
extern crate regex;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process;

use getopts::Options;

pub mod assembler;
use assembler::assembler::*;
use assembler::data_range;
use assembler::i8hex_writer;
use assembler::parser::*;
use assembler::scanner::Scanner;

// MARK: - Assembler

fn assemble_file<F>(input_path: &str, output_path: &str, log_error: F) where F : Fn(String) -> () {

  // Open the input file (read-only).
  let mut input_file = File::open(input_path).unwrap_or_else(|reason| {
    log_error(format!("Unable to open input file '{}'", input_path));
    log_error(reason.to_string());
    process::exit(1);
  });

  let mut buffer = String::new();

  // Read the contents of the input file into the buffer.
  input_file.read_to_string(&mut buffer).unwrap_or_else(|reason| {
    log_error(format!("Unable to read contents of input file '{}'", input_path));
    log_error(reason.to_string());
    process::exit(1);
  });

  // Create a scanner over the input.
  let scanner = Scanner::new(input_path, buffer.as_str());

  // Parse the input into an abstract syntax list form. Errors are pre-formatted.
  let syntax_list = parse(scanner).unwrap_or_else(|message| {
    println!("{}", message);
    process::exit(1);
  });

  // Assemble the syntax list into data ranges. Errors are pre-formatted.
  let data_ranges = assemble(syntax_list).unwrap_or_else(|message| {
    println!("{}", message);
    process::exit(1);
  });

  // TODO: Convert find_overlapping_data_ranges to take &[DataRange].
  // Convert the ranges a vector of references to the ranges themselves.
  let data_range_refs = data_ranges.iter().map(|range| range).collect::<Vec<_>>();

  // Verify no overlapping data ranges exist in the result.
  let overlapping_ranges = data_range::find_overlapping_data_ranges(data_range_refs.as_slice());
  if overlapping_ranges.len() > 0 {
    log_error(String::from("Input file specifies overlapping data ranges."));
    log_error(String::from("Data from one section has overflowed into the adjacent."));

    // Emit overlapping data range information.
    for (a, b) in overlapping_ranges {
      let range_of_a = a.address_range();
      let range_of_b = b.address_range();
      let (a_first, a_last) = (range_of_a.start.as_usize(), range_of_a.end.as_usize() - 1);
      let (b_first, b_last) = (range_of_b.start.as_usize(), range_of_b.end.as_usize() - 1);
      log_error(format!("  range ${:03X}...${:03X} overlaps with:", a_first, a_last));
      log_error(format!("  range ${:03X}...${:03X}", b_first, b_last));
    }

    process::exit(1);
  }

  // Convert the data ranges to Intel I8HEX format.
  let result = i8hex_writer::i8hex_representation_of_data_ranges(data_range_refs.as_slice());

  // Open the output file for writing (truncate).
  let mut output_file = File::create(output_path).unwrap_or_else(|reason| {
    log_error(format!("Unable to create output file '{}'", output_path));
    log_error(reason.to_string());
    process::exit(1);
  });

  // Write the result to the output file.
  output_file.write_all(result.as_bytes()).unwrap_or_else(|reason| {
    log_error(format!("Unable to write result to output file '{}'", output_path));
    log_error(reason.to_string());
    process::exit(1);
  });
}

// MARK: - Option Processing

/**
 Displays usage information for the tool.
 @param program The path to the executable as invoked (argv[0]).
 @param options A fully-constructed options bundle for the tool.
 */
fn print_usage(program: &str, options: Options) {
  let brief = format!("USAGE: {} [options] <input>", program);
  print!("{}", options.usage(&brief));
}

/**
 Displays the program name and version information.
 */
fn print_version() {
  println!("c8s version {}", env!("CARGO_PKG_VERSION"));
}

fn main() {
  let arguments: Vec<String> = env::args().collect();
  let program = arguments[0].clone();

  let mut options = Options::new();
  options.optopt ("o", "output",  "Write output to <file>", "<file>");
  options.optflag("h", "help",    "Display available options");
  options.optflag("v", "version", "Display version information");

  // Parse the command-line arguments and exit in the event an invalid argument is encountered.
  let matches = options.parse(&arguments[1..]).unwrap_or_else(|reason| {
    println!("{}: error: {}", program, reason.to_string());
    process::exit(1);
  });

  // Help requested.
  if matches.opt_present("h") {
    print_usage(&program, options);
    return;
  }

  // Version information requested.
  if matches.opt_present("v") {
    print_version();
    return;
  }

  // Extract the output file name, and default to 'a.hex' if not specified.
  let output_path = &matches.opt_str("o").unwrap_or(String::from("a.hex"));

  // Allow one single input file name parameter.
  let input_path = match matches.free.len() {
    1 => &matches.free[0],
    _ => {
      print_usage(&program, options);
      process::exit(1);
    }
  };

  // Execute the assembler, in the event of failure, log an error and exit.
  assemble_file(input_path, output_path, |reason| {
    println!("{}: error: {}", program, reason);
  });
}
