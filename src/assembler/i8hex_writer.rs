
use assembler::data_range::DataRange;

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

// MARK: - Tests

#[cfg(test)]
mod tests {
  
  use super::*;
  use super::{format_record, ihex_checksum, type_codes};

  #[test]
  fn test_ihex_checksum() {
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

}
