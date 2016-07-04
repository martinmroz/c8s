//
// Copyright 2016 The c8s Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>.
// All files in the project carrying such notice may not be copied, modified, or 
// distributed except according to those terms.
//

use std::fmt;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SourceFileLocation<'a> {
  /// The display name of the source file (usually last path component).
  file_name: &'a str,
  /// The line number corresponding to the range.
  line: usize,
  /// The offset from the start of input, in bytes, starting at 1.
  location: usize,
  /// The length of the region, in bytes.
  length: usize
}

impl<'a> fmt::Display for SourceFileLocation<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self.length {
      1 => write!(f, "{}:{}:{}",    self.file_name, self.line, self.location),
      _ => write!(f, "{}:{}:{}-{}", self.file_name, self.line, self.location, self.location + self.length - 1)
    }
  }
}

impl<'a> SourceFileLocation<'a> {
  /**
   Creates a new source file location.
   @param file_name The display name of the source file (usually last path component).
   @param line The line number within the source file, starting at 1. Asserts if 0.
   @param location The offset from the start of the input, in code points, starting at 1. Asserts if 0.
   @param length The number of code points corresponding to the location. Asserts if 0.
   */
  pub fn new(file_name: &'a str, line: usize, location: usize, length: usize) -> Self {
    assert!(line > 0 && location > 0 && length > 0);
    SourceFileLocation {
      file_name: file_name,
      line: line,
      location: location,
      length: length
    }
  }
}

#[cfg(test)]
mod tests {
  
  use super::*;

  #[test]
  fn test_display_fmt_one_character() {
    let one_length_location = SourceFileLocation::new("test_file.s", 1, 1, 1);
    assert_eq!(format!("{}", one_length_location), "test_file.s:1:1");
  }

  #[test]
  fn test_display_fmt_many_characters() {
    let two_length_location = SourceFileLocation::new("test_file.s", 2, 3, 2);
    assert_eq!(format!("{}", two_length_location), "test_file.s:2:3-4");
    let five_length_location = SourceFileLocation::new("test_file.s", 2, 3, 5);
    assert_eq!(format!("{}", five_length_location), "test_file.s:2:3-7");
  }

  #[test]
  fn test_display_fmt_empty_file_name() {
    let location_one_length = SourceFileLocation::new("", 1, 1, 1);
    assert_eq!(format!("{}", location_one_length), ":1:1");
    let location_two_length = SourceFileLocation::new("", 1, 2, 3);
    assert_eq!(format!("{}", location_two_length), ":1:2-4");
  }

  #[test]
  #[should_panic]
  fn test_new_asserts_on_zero_line() {
    let _ = SourceFileLocation::new("test.s", 0, 2, 3);
  }

  #[test]
  #[should_panic]
  fn test_new_asserts_on_zero_length() {
    let _ = SourceFileLocation::new("test.s", 1, 0, 3);
  }

  #[test]
  #[should_panic]
  fn test_new_asserts_on_zero_location() {
    let _ = SourceFileLocation::new("test.s", 1, 2, 0);
  }

}
