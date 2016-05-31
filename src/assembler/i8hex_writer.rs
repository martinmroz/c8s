
use assembler::data_range;
use assembler::data_range::DataRange;

// MARK: - Intel HEX Formatting Utilities

enum Record<'a> {
  /// A data record. Supported in i8, i16 and i32.
  Data { address: u16, value: &'a [u8] },
  /// An end-of-file record. Supported in i8, i16 and i32.
  EndOfFile
}

mod type_codes {
  /// Type code for a Data record.
  pub const DATA_TYPE_CODE: u8 = 0x00;
  /// Type code for an End-of-File record.
  pub const END_OF_FILE_TYPE_CODE: u8 = 0x01;
}

impl<'a> Record<'a> {
  /**
   @return String value of the Intel HEX record for the receiver, excluding a trailing newline.
   */
  pub fn as_formatted_string(&self) -> String {
    match self {
      &Record::Data {address, value} => format_record(address, value, type_codes::DATA_TYPE_CODE),
      &Record::EndOfFile             => format_record(0x0000 , &[],   type_codes::END_OF_FILE_TYPE_CODE)
    }
  }
}

/**
 An Intel HEX record is composed of the following elements:
 +----------------+-----------------+---------------+-----------+---------+---------------+
 | Start Code (:) | Byte Count (u8) | Address (u16) | Type (u8) | Data... | Checksum (u8) |
 +----------------+-----------------+---------------+-----------+---------+---------------+
 */
fn format_record<'a>(address: u16, data: &'a [u8], type_code: u8) -> String {
  assert!(data.len() <= 255);

  // Construct the checksum region.
  let length_of_checksum_region = 4 + data.len();
  let mut checksum_region = Vec::<u8>::with_capacity(length_of_checksum_region);
  checksum_region.push(data.len() as u8);
  checksum_region.push(((address & 0xFF00) >> 8) as u8);
  checksum_region.push(((address & 0x00FF) >> 0) as u8);
  checksum_region.push(type_code);
  checksum_region.extend_from_slice(data);
  let checksum = ihex_checksum(checksum_region.as_slice());

  // Construct the record, excluding the start code and checksum.
  let checksum_region_string = 
    checksum_region
      .iter()
      .map(|byte: &u8| format!("{:02X}", byte))
      .fold(String::new(), |acc, value| acc + &value);
  
  // Construct the formatted version of the record.
  format!(":{}{:02X}", checksum_region_string, checksum)
}

/**
 The Intel HEX checksum is computed as the two's complement of the sum of all
 elements preceding, excluding the Start Code. This is the Byte Count, Address,
 Record Type and Data.
 @param data Slice view of the checksum region.
 @return The Intel HEX checksum of the supplied region.
 */
fn ihex_checksum<'a>(data: &'a [u8]) -> u8 {
  let sum: usize = data.iter().fold(0, |acc, &value| acc.wrapping_add(value as usize));
  (((sum & 0xFF) as u8) ^ 0xFF).wrapping_add(1)
}

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
      records.push( Record::Data { address: address_for_record, value: data_for_record } );

      // Increment the address by the number of bytes emitted in the record.
      address_for_record += data_for_record.len() as u16;
    }
  }

  // All I8HEX files end in an EOF marker.
  records.push(Record::EndOfFile);

  // Obtain the formatted representation of each record and join with newlines for display.
  records
    .iter()
    .map(|record| record.as_formatted_string())
    .collect::<Vec<_>>()
    .join("\n")
}

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use super::Record;
  use super::{format_record, i8hex_representation_of_data_ranges, ihex_checksum};
  use super::type_codes;

  use assembler::data_range::DataRange;
  use assembler::u12::*;

  #[test]
  fn test_ihex_checksum() {
    assert_eq!(ihex_checksum(&vec![]), 0x00);
    assert_eq!(ihex_checksum(&vec![0x10,0x01,0x00,0x00,0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01]), 0x40);
    assert_eq!(ihex_checksum(&vec![0x10,0x01,0x10,0x00,0x21,0x46,0x01,0x7E,0x17,0xC2,0x00,0x01,0xFF,0x5F,0x16,0x00,0x21,0x48,0x01,0x19]), 0x28);
    assert_eq!(ihex_checksum(&vec![0x10,0x01,0x20,0x00,0x19,0x4E,0x79,0x23,0x46,0x23,0x96,0x57,0x78,0x23,0x9E,0xDA,0x3F,0x01,0xB2,0xCA]), 0xA7);
    assert_eq!(ihex_checksum(&vec![0x10,0x01,0x30,0x00,0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21]), 0xC7);
    assert_eq!(ihex_checksum(&vec![0x00,0x00,0x00,0x01]), 0xFF);
  }

  #[test]
  fn test_format_record() {
    assert_eq!(format_record(0x0100, &vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01], type_codes::DATA_TYPE_CODE), String::from(":10010000214601360121470136007EFE09D2190140"));
    assert_eq!(format_record(0x0110, &vec![0x21,0x46,0x01,0x7E,0x17,0xC2,0x00,0x01,0xFF,0x5F,0x16,0x00,0x21,0x48,0x01,0x19], type_codes::DATA_TYPE_CODE), String::from(":100110002146017E17C20001FF5F16002148011928"));
    assert_eq!(format_record(0x0120, &vec![0x19,0x4E,0x79,0x23,0x46,0x23,0x96,0x57,0x78,0x23,0x9E,0xDA,0x3F,0x01,0xB2,0xCA], type_codes::DATA_TYPE_CODE), String::from(":10012000194E79234623965778239EDA3F01B2CAA7"));
    assert_eq!(format_record(0x0130, &vec![0x3F,0x01,0x56,0x70,0x2B,0x5E,0x71,0x2B,0x72,0x2B,0x73,0x21,0x46,0x01,0x34,0x21], type_codes::DATA_TYPE_CODE), String::from(":100130003F0156702B5E712B722B732146013421C7"));
    assert_eq!(format_record(0x0000, &[], type_codes::END_OF_FILE_TYPE_CODE), String::from(":00000001FF"));
  }

  #[test]
  fn test_record_as_formatted_string() {
    let eof_record = Record::EndOfFile;
    assert_eq!(eof_record.as_formatted_string(), String::from(":00000001FF"));

    let data_record = Record::Data { address: 0x0100, value: &vec![0x21,0x46,0x01,0x36,0x01,0x21,0x47,0x01,0x36,0x00,0x7E,0xFE,0x09,0xD2,0x19,0x01] };
    assert_eq!(data_record.as_formatted_string(), String::from(":10010000214601360121470136007EFE09D2190140"));
  }

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
