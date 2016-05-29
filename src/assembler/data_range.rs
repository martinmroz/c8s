
use std::ops::Range;

use assembler::u12;
use assembler::u12::*;

#[derive(PartialEq, Eq, Debug)]
pub struct DataRange {
  /// The first address populated by the bytes in the range.
  start_address: U12,
  /// The bytes occupying the range.
  data: Vec<u8>
}

impl DataRange {
  
  /**
   @param start_address The address of the first element in the data range in the result.
   @return A new data range from the start address with zero elements.
   */
  pub fn new(start_address: U12) -> Self {
    DataRange {
      start_address: start_address,
      data: Vec::new()
    }
  }

  /**
   @return The number of bytes in the data range.
   */
  pub fn len(&self) -> usize {
    self.data.len()
  }

  /**
   @return An immutable reference to the data underlying the range.
   */
  pub fn data(&self) -> &Vec<u8> {
    &self.data
  }

  /**
   @return A half-open range { address | start <= address < end } representing addresses in the range.
   */
  pub fn address_range(&self) -> Range<U12> {    
    Range {
      start: self.start_address,
        end: self.start_address.wrapping_add(self.len().as_u12().unwrap())
    }
  }

  /**
   @return The number of bytes remaining to store data in the range.
   */
  pub fn bytes_remaining(&self) -> usize {
    let total_space_in_range = (u12::MAX - self.start_address).as_usize() + 1;
    total_space_in_range - self.len()
  }

  /**
   Appends the slice to the end of the data range. Checks for capacity in advance.
   @param data The bytes to append to the data range.
   */
  pub fn append(&mut self, data: &[u8]) -> bool {
    if self.bytes_remaining() >= data.len() {
      self.data.extend_from_slice(data);
      true
    } else {
      false
    }
  }

}

#[cfg(test)]
mod tests {

  use super::*;

  use assembler::u12;
  use assembler::u12::*;

  #[test]
  fn test_empty_ranges() {
    let zero_range = DataRange::new(U12::zero());
    assert_eq!(zero_range.len(), 0);
    assert_eq!(zero_range.bytes_remaining(), 4096);
    assert_eq!(zero_range.address_range(), U12::zero() .. U12::zero());

    let half_address = 0x800.as_u12().unwrap();
    let half_range = DataRange::new(half_address);
    assert_eq!(half_range.len(), 0);
    assert_eq!(half_range.bytes_remaining(), 2048);
    assert_eq!(half_range.address_range(), half_address .. half_address);

    let max_range = DataRange::new(u12::MAX);
    assert_eq!(max_range.len(), 0);
    assert_eq!(max_range.bytes_remaining(), 1);
    assert_eq!(max_range.address_range(), u12::MAX .. u12::MAX);
  }

  #[test]
  fn test_append_within_range() {
    let program_space_start = 0x200.as_u12().unwrap();
    let mut program_space = DataRange::new(program_space_start);
    assert_eq!(program_space.len(), 0);
    assert_eq!(program_space.bytes_remaining(), 3584);
    assert_eq!(program_space.address_range(), program_space_start .. program_space_start);

    // Append a single byte to the data range.
    let one_byte_slice: &[u8] = &[1];
    assert_eq!(program_space.append(one_byte_slice), true);
    assert_eq!(program_space.len(), 1);
    assert_eq!(program_space.bytes_remaining(), 3583);
    assert_eq!(program_space.address_range(), program_space_start .. program_space_start.wrapping_add(U12::from(1)));

    // Append a single byte to the data range.
    let two_byte_slice: &[u8] = &[2, 3];
    assert_eq!(program_space.append(two_byte_slice), true);
    assert_eq!(program_space.len(), 3);
    assert_eq!(program_space.bytes_remaining(), 3581);
    assert_eq!(program_space.address_range(), program_space_start .. program_space_start.wrapping_add(U12::from(3)));

    // Validate the data was appended.
    assert_eq!(program_space.data(), &vec![1,2,3]);
  }

  #[test]
  fn test_append_out_of_range() {
    let mut max_range = DataRange::new(u12::MAX);
    let one_byte_slice: &[u8] = &[1];

    // Append data into the only slot in the data range.
    assert_eq!(max_range.append(one_byte_slice), true);
    assert_eq!(max_range.len(), 1);
    assert_eq!(max_range.bytes_remaining(), 0);
    assert_eq!(max_range.address_range(), u12::MAX .. u12::MIN);

    // Validate the data was appended.
    assert_eq!(max_range.data(), &vec![1]);

    // Attempt to apppend past the end of the data range.
    assert_eq!(max_range.append(one_byte_slice), false);
    assert_eq!(max_range.len(), 1);
    assert_eq!(max_range.bytes_remaining(), 0);
    assert_eq!(max_range.address_range(), u12::MAX .. u12::MIN);

    // Validate the data was not appended.
    assert_eq!(max_range.data(), &vec![1]);
  }

}
