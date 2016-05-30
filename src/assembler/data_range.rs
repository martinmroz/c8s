
use std::cmp;
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

  /**
   @return YES if the supplied range intersects the receiver.
   */
  pub fn intersects(&self, range: &DataRange) -> bool {
    let lhs_range =  self.address_range();
    let rhs_range = range.address_range();

    // Ranges intersect if the greater of the two starts is less than the smaller of the two ends (half-open).
    cmp::max(lhs_range.start, rhs_range.start) < cmp::min(lhs_range.end, rhs_range.end)
  }

}

// MARK: - Public Functions

/**
 @param ranges A list of data ranges.
 @return A list of pairs of overlapping data ranges.
 */
pub fn find_overlapping_data_ranges<'a>(ranges: &'a [DataRange]) -> Vec<(&'a DataRange, &'a DataRange)> {
  let mut overlapping_ranges = Vec::new();

  /*
   Compare each data range to each other data range. This is done by evaluating the mathematical
   combination pairs of the data range (order does not matter, pairs must be unique). This has
   complexity O(ranges.len() nCr 2).
   */
  for i in 0..ranges.len() {
    for j in i+1..ranges.len() {
      let lhs = &ranges[i];
      let rhs = &ranges[j];

      if lhs.intersects(rhs) {
        overlapping_ranges.push((lhs, rhs));
      }
    }
  }

  overlapping_ranges
}

// MARK: - Tests

#[cfg(test)]
mod tests {

  use std::iter::repeat;

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

  #[test]
  fn test_intersects() {
    // Case 1: Obviously disjoint (+++++===++), distance: 3.
    let disjoint_1a = range_with_start_and_length(0, 5);
    let disjoint_1b = range_with_start_and_length(8, 2);
    assert_eq!(disjoint_1a.intersects(&disjoint_1b), false);
    assert_eq!(disjoint_1b.intersects(&disjoint_1a), false);

    // Case 2: Tightly disjoint (+++++===++), distance: 1.
    let disjoint_2a = range_with_start_and_length(0, 5);
    let disjoint_2b = range_with_start_and_length(6, 4);
    assert_eq!(disjoint_2a.intersects(&disjoint_2b), false);
    assert_eq!(disjoint_2b.intersects(&disjoint_2a), false);

    // Case 3: Adjacent (++++++++++), distance: 0.
    let adjacent_a = range_with_start_and_length(0, 5);
    let adjacent_b = range_with_start_and_length(5, 5);
    assert_eq!(adjacent_a.intersects(&adjacent_b), false);
    assert_eq!(adjacent_b.intersects(&adjacent_a), false);

    // Case 4: Tail Overlap (++++++v+++) distance: -1.
    let tail_overlap_a = range_with_start_and_length(0, 7);
    let tail_overlap_b = range_with_start_and_length(6, 4);
    assert_eq!(tail_overlap_a.intersects(&tail_overlap_b), true);
    assert_eq!(tail_overlap_b.intersects(&tail_overlap_a), true);

    // Case 5: Wholly Contained Head (v+++++++++) distance: -1.
    let wholly_contained_head_a = range_with_start_and_length(0, 10);
    let wholly_contained_head_b = range_with_start_and_length(0, 1);
    assert_eq!(wholly_contained_head_a.intersects(&wholly_contained_head_b), true);
    assert_eq!(wholly_contained_head_b.intersects(&wholly_contained_head_a), true);

    // Case 6: Wholly Contained Tail (+++++++++v) distance: -1.
    let wholly_contained_tail_a = range_with_start_and_length(0, 10);
    let wholly_contained_tail_b = range_with_start_and_length(9, 1);
    assert_eq!(wholly_contained_tail_a.intersects(&wholly_contained_tail_b), true);
    assert_eq!(wholly_contained_tail_b.intersects(&wholly_contained_tail_a), true);

    // Case 7: Middle of Range (++++vv++++) distance: -2.
    let middle_a = range_with_start_and_length(0, 10);
    let middle_b = range_with_start_and_length(4, 2);
    assert_eq!(middle_a.intersects(&middle_b), true);
    assert_eq!(middle_b.intersects(&middle_a), true);

    // Case 7: Zero Length Range (++++++++++) distance: 0.
    let zero_a = range_with_start_and_length(0, 10);
    let zero_b = range_with_start_and_length(4, 0);
    assert_eq!(zero_a.intersects(&zero_b), false);
    assert_eq!(zero_b.intersects(&zero_a), false);
  }

  // MARK: - Helper Methods

  /// Construct a new zero-filled range at the start address provided with the length provided.
  fn range_with_start_and_length(start: usize, length: usize) -> DataRange {
    let mut range = DataRange::new(start.as_u12().unwrap());
    let data: Vec<u8> = repeat(0u8).take(length).collect();
    range.append(data.as_slice());
    range
  }

}
