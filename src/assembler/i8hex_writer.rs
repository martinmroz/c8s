
use ihex::record::Record;
use ihex::writer;

use assembler::data_range;
use assembler::data_range::DataRange;

// MARK: - Public API

/**
 Converts the provided data ranges into a single Intel HEX record. These data ranges
 cannot overlap and still yield a valid I8HEX record. It is the responsibility of
 the caller to ensure this does not happen.
 @param ranges The data ranges to convert to I8HEX format.
 @return The complete I8HEX record (including EOF) of the given data ranges.
 */
pub fn i8hex_representation_of_data_ranges<'a>(ranges: &'a [&'a DataRange]) -> String {
  assert!(data_range::find_overlapping_data_ranges(ranges).len() == 0);

  // All records are collected into a list.
  let mut records = Vec::<Record>::new();

  for range in ranges {

    // The range will be sub-divded into chunks of up to 16 bytes, so sub-address must be tracked.
    let mut address_for_record = range.address_range().start.as_u16();

    // Iterate over the data range in groups of up to 16.
    for data_for_record in range.data().as_slice().chunks(16) {

      // Create a new data record for the chunk.
      records.push( Record::Data { offset: address_for_record, value: Vec::from(data_for_record) } );

      // Increment the address by the number of bytes emitted in the record.
      address_for_record += data_for_record.len() as u16;
    }
  }

  // All I8HEX files end in an EOF marker.
  records.push(Record::EndOfFile);

  // Obtain the formatted representation of each record and join with newlines for display.
  writer::create_object_file_representation(records.as_slice()).unwrap() + &"\n"
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use super::i8hex_representation_of_data_ranges;

  use assembler::data_range::DataRange;
  use assembler::u12::*;

  #[test]
  fn test_i8hex_representation_of_data_ranges_no_ranges() {
    // An empty set of data ranges yields just an EOF marker.
    assert_eq!(i8hex_representation_of_data_ranges(&[]), String::from(":00000001FF"));
  }

  #[test]
  fn test_i8hex_representation_of_data_ranges_one_range() {
    // Build an average-case I8HEX record.
    let mut data_range = DataRange::new(0x100.as_u12().unwrap());
    data_range.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    data_range.append(&vec![0x21,0x46,0x01,0x7E,0x17,0xC2,0x00,0x01,0xFF,0x5F,0x16,0x00,0x21,0x48,0x01,0x19]);
    data_range.append(&vec![0x19,0x4E,0x79,0x23,0x46,0x23,0x96,0x57,0x78,0x23,0x9E,0xDA,0x3F,0x01,0xB2,0xCA]);
    data_range.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21]);

    // Validate the average case yielded the anticipated result.
    let i8hex_rep_average = i8hex_representation_of_data_ranges(&[&data_range]);
    let expected_i8hex_rep_average = String::new() +
      &":10010000214601360121470136007EFE09D2190140\n" + 
      &":100110002146017E17C20001FF5F16002148011928\n" +
      &":10012000194E79234623965778239EDA3F01B2CAA7\n" +
      &":100130003F0156702B5E712B722B732146013421C7\n" +
      &":00000001FF";
    assert_eq!(i8hex_rep_average, expected_i8hex_rep_average);
  }

  #[test]
  fn test_i8hex_representation_of_data_ranges_adjacent_ranges() {
    // Build a pair of adjacent data ranges.
    let mut range_a = DataRange::new(0x100.as_u12().unwrap());
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    let mut range_b = DataRange::new(0x110.as_u12().unwrap());
    range_b.append(&vec![0x21,0x46,0x01,0x7E,0x17,0xC2,0x00,0x01,0xFF,0x5F,0x16,0x00,0x21,0x48,0x01,0x19]);

    // Validate the average case yielded the anticipated result.
    let i8hex_rep_adjacent = i8hex_representation_of_data_ranges(&[&range_a, &range_b]);
    let expected_i8hex_rep_adjacent = String::new() +
      &":10010000214601360121470136007EFE09D2190140\n" + 
      &":100110002146017E17C20001FF5F16002148011928\n" +
      &":00000001FF";
    assert_eq!(i8hex_rep_adjacent, expected_i8hex_rep_adjacent);
  }

  #[test]
  fn test_i8hex_representation_of_data_ranges_disjoint_ranges() {
    // Build an disjoint pair of data ranges.
    let mut range_a = DataRange::new(0x100.as_u12().unwrap());
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    let mut range_b = DataRange::new(0x130.as_u12().unwrap());
    range_b.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21]);

    // Validate the average case yielded the anticipated result.
    let i8hex_rep_disjoint = i8hex_representation_of_data_ranges(&[&range_a, &range_b]);
    let expected_i8hex_rep_disjoint = String::new() +
      &":10010000214601360121470136007EFE09D2190140\n" + 
      &":100130003F0156702B5E712B722B732146013421C7\n" +
      &":00000001FF";
    assert_eq!(i8hex_rep_disjoint, expected_i8hex_rep_disjoint);
  }

  #[test]
  fn test_i8hex_representation_of_data_ranges_uneven_ranges() {
    // Build an uneven pair of data ranges.
    let mut range_a = DataRange::new(0x100.as_u12().unwrap());
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19]);
    let mut range_b = DataRange::new(0x130.as_u12().unwrap());
    range_b.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21,0x22]);

    // Validate the average case yielded the anticipated result.
    let i8hex_rep = i8hex_representation_of_data_ranges(&[&range_a, &range_b]);
    let expected_i8hex_rep = String::new() +
      &":0F010000214601360121470136007EFE09D21942\n" + 
      &":100130003F0156702B5E712B722B732146013421C7\n" +
      &":01014000229C\n" +
      &":00000001FF";
    assert_eq!(i8hex_rep, expected_i8hex_rep);
  }

  #[test]
  #[should_panic]
  fn test_i8hex_representation_of_data_ranges_panics_when_overlapping() {
    // Build an overlapping pair of ranges.
    let mut range_a = DataRange::new(0x100.as_u12().unwrap());
    range_a.append(&vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]);
    let mut range_b = DataRange::new(0x101.as_u12().unwrap());
    range_b.append(&vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21]);
    i8hex_representation_of_data_ranges(&[&range_a, &range_b]);
  }

}
