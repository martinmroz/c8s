//
// Copyright 2016 The c8s Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>.
// All files in the project carrying such notice may not be copied, modified, or 
// distributed except according to those terms.
//

use std::cell::RefCell;

use ihex::record::Record;
use ihex::writer;

use assembler::data_range;
use assembler::data_range::DataRange;

// MARK: - Public API

/**
 Converts the provided data ranges into a single Intel HEX record. These data ranges
 cannot overlap and still yield a valid ihex record. It is the responsibility of
 the caller to ensure this does not happen.
 @param ranges The data ranges to convert to ihex format.
 @return The complete ihex record (including EOF) of the given data ranges.
 */
pub fn ihex_representation_of_data_ranges<'a>(ranges: &'a [DataRange]) -> String {
  assert!(data_range::find_overlapping_ranges(ranges).len() == 0);

  // All records are collected into a list.
  let mut records = Vec::<Record>::new();

  for range in ranges.iter() {

    // The range will be sub-divded into chunks of up to 16 bytes, so sub-address must be tracked.
    let record_address = RefCell::new(u16::from(range.address_range().start));

    // Sub-divide the range into 16-byte Record::Data objects.
    records.append(
      &mut range
        // Inspect the data in the range.
        .data()
        // As a u8 slice.
        .as_slice()
        // In groups of 16 bytes.
        .chunks(16)
        // Create a tuple of (Length, Record).
        .map(|chunk| {
          (chunk.len() as u16, Record::Data { offset: *record_address.borrow(), value: Vec::from(chunk) })
        })
        // Increment the address counter by the number of bytes incorporated into the record.
        .inspect(|&(length, _)| {
          *record_address.borrow_mut() += length;
        })
        // Discard the length from the tuple.
        .map(|(_, record)| record)
        // Collect the records into a Vec<Record>.
        .collect::<Vec<_>>()
    );

  }

  // All ihex files end in an EOF marker.
  records.push(Record::EndOfFile);

  // Obtain the formatted representation of each record and join with newlines for display.
  writer::create_object_file_representation(records.as_slice()).unwrap() + &"\n"
}

// MARK: - Tests

#[cfg(test)]
mod tests {

  use twelve_bit::u12::*;

  use assembler::data_range::DataRange;

  use super::ihex_representation_of_data_ranges;

  #[test]
  fn test_ihex_representation_of_data_ranges_no_ranges() {
    // An empty set of data ranges yields just an EOF marker.
    assert_eq!(ihex_representation_of_data_ranges(&[]), String::from(":00000001FF\n"));
  }

  #[test]
  fn test_ihex_representation_of_data_ranges_one_range() {
    // Build an average-case ihex record.
    let mut data_range = DataRange::new(u12![0x100]);
    data_range.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    data_range.append(&vec![0x21,0x46,0x01,0x7E,0x17,0xC2,0x00,0x01,0xFF,0x5F,0x16,0x00,0x21,0x48,0x01,0x19]);
    data_range.append(&vec![0x19,0x4E,0x79,0x23,0x46,0x23,0x96,0x57,0x78,0x23,0x9E,0xDA,0x3F,0x01,0xB2,0xCA]);
    data_range.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21]);

    // Validate the average case yielded the anticipated result.
    let ihex_rep_average = ihex_representation_of_data_ranges(&[data_range]);
    let expected_ihex_rep_average = String::new() +
      &":10010000214601360121470136007EFE09D2190140\n" + 
      &":100110002146017E17C20001FF5F16002148011928\n" +
      &":10012000194E79234623965778239EDA3F01B2CAA7\n" +
      &":100130003F0156702B5E712B722B732146013421C7\n" +
      &":00000001FF\n";
    assert_eq!(ihex_rep_average, expected_ihex_rep_average);
  }

  #[test]
  fn test_ihex_representation_of_data_ranges_adjacent_ranges() {
    // Build a pair of adjacent data ranges.
    let mut range_a = DataRange::new(u12![0x100]);
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    let mut range_b = DataRange::new(u12![0x110]);
    range_b.append(&vec![0x21,0x46,0x01,0x7E,0x17,0xC2,0x00,0x01,0xFF,0x5F,0x16,0x00,0x21,0x48,0x01,0x19]);

    // Validate the average case yielded the anticipated result.
    let ihex_rep_adjacent = ihex_representation_of_data_ranges(&[range_a, range_b]);
    let expected_ihex_rep_adjacent = String::new() +
      &":10010000214601360121470136007EFE09D2190140\n" + 
      &":100110002146017E17C20001FF5F16002148011928\n" +
      &":00000001FF\n";
    assert_eq!(ihex_rep_adjacent, expected_ihex_rep_adjacent);
  }

  #[test]
  fn test_ihex_representation_of_data_ranges_disjoint_ranges() {
    // Build an disjoint pair of data ranges.
    let mut range_a = DataRange::new(u12![0x100]);
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    let mut range_b = DataRange::new(u12![0x130]);
    range_b.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21]);

    // Validate the average case yielded the anticipated result.
    let ihex_rep_disjoint = ihex_representation_of_data_ranges(&[range_a, range_b]);
    let expected_ihex_rep_disjoint = String::new() +
      &":10010000214601360121470136007EFE09D2190140\n" + 
      &":100130003F0156702B5E712B722B732146013421C7\n" +
      &":00000001FF\n";
    assert_eq!(ihex_rep_disjoint, expected_ihex_rep_disjoint);
  }

  #[test]
  fn test_ihex_representation_of_data_ranges_uneven_ranges() {
    // Build an uneven set of data ranges.
    let mut range_a = DataRange::new(u12![0x100]);
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19]);
    let mut range_b = DataRange::new(u12![0x130]);
    range_b.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21,0x22]);
    let mut range_c = DataRange::new(u12![0x200]);
    range_c.append(&vec![0x3F]);

    // Validate the average case yielded the anticipated result.
    let ihex_rep = ihex_representation_of_data_ranges(&[range_a, range_b, range_c]);
    let expected_ihex_rep = String::new() +
      &":0F010000214601360121470136007EFE09D21942\n" + 
      &":100130003F0156702B5E712B722B732146013421C7\n" +
      &":01014000229C\n" +
      &":010200003FBE\n" +
      &":00000001FF\n";
    assert_eq!(ihex_rep, expected_ihex_rep);
  }

  #[test]
  #[should_panic]
  fn test_ihex_representation_of_data_ranges_panics_when_overlapping() {
    // Build an overlapping pair of ranges.
    let mut range_a = DataRange::new(u12![0x100]);
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    let mut range_b = DataRange::new(u12![0x101]);
    range_b.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21]);
    ihex_representation_of_data_ranges(&[range_a, range_b]);
  }

}
