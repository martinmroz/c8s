
use std::fmt;

use assembler::u12::*;

#[allow(non_camel_case_types)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Opcode {
  NOP             ,
  CLS             ,
  RET             ,
  TRAP            ,
  TRAPRET         ,
  JP              { target: U12 },
  CALL            { target: U12 },
  SE_IMMEDIATE    { register_x: u8, value: u8 },
  SNE_IMMEDIATE   { register_x: u8, value: u8 },
  SE_REGISTER     { register_x: u8, register_y: u8 },
  SNE_REGISTER    { register_x: u8, register_y: u8 },
  LD_IMMEDIATE    { register_x: u8, value: u8 },
  ADD_IMMEDIATE   { register_x: u8, value: u8 },
  LD_REGISTER     { register_x: u8, register_y: u8 },
  OR              { register_x: u8, register_y: u8 },
  AND             { register_x: u8, register_y: u8 },
  XOR             { register_x: u8, register_y: u8 },
  ADD_REGISTER    { register_x: u8, register_y: u8 },
  SUB             { register_x: u8, register_y: u8 },
  SHR             { register_x: u8, register_y: u8 },
  SUBN            { register_x: u8, register_y: u8 },
  SHL             { register_x: u8, register_y: u8 }, 
  LD_I            { value: U12 },
  JP_V0           { value: U12 },
  RND             { register_x: u8, mask: u8 },
  DRW             { register_x: u8, register_y: u8, bytes: u8 },
  SE_K            { register_x: u8 },
  SNE_K           { register_x: u8 },
  LD_X_DT         { register_x: u8 },
  LD_X_K          { register_x: u8 },
  LD_DT_X         { register_x: u8 },
  LD_ST_X         { register_x: u8 },
  ADD_I_X         { register_x: u8 },
  SPRITE_I        { register_x: u8 },
  BCD_I           { register_x: u8 },
  SAVE_I          { register_x: u8 },
  RESTORE_I       { register_x: u8 }
}

impl Opcode {

  pub fn from_instruction(instruction: u16) -> Opcode {
    let nibbles = (
      ((instruction & 0xF000) >> 12) as u8,
      ((instruction & 0x0F00) >>  8) as u8,
      ((instruction & 0x00F0) >>  4) as u8,
      ((instruction & 0x000F) >>  0) as u8
    );

    let imm12 = (instruction & 0x0FFF).as_u12().unwrap();
    let imm8: u8 = (instruction & 0x00FF) as u8;
    let imm4: u8 = (instruction & 0x000F) as u8;
    let register_x = nibbles.1;
    let register_y = nibbles.2;

    match nibbles {
      (0x0, 0x0, 0xE, 0x0) => { Opcode::CLS           }
      (0x0, 0x0, 0xE, 0xE) => { Opcode::RET           }
      (0x0, 0x0, 0xF, 0x9) => { Opcode::TRAPRET       }
      (0x0, 0x0, 0xF, 0xA) => { Opcode::TRAP          }
      (0x1, _,   _,   _  ) => { Opcode::JP            { target: imm12 } }
      (0x2, _,   _,   _  ) => { Opcode::CALL          { target: imm12 } }
      (0x3, _,   _,   _  ) => { Opcode::SE_IMMEDIATE  { register_x: register_x, value: imm8 } }
      (0x4, _,   _,   _  ) => { Opcode::SNE_IMMEDIATE { register_x: register_x, value: imm8 } }
      (0x5, _,   _,   0x0) => { Opcode::SE_REGISTER   { register_x: register_x, register_y: register_y } }
      (0x6, _,   _,   _  ) => { Opcode::LD_IMMEDIATE  { register_x: register_x, value: imm8 } }
      (0x7, _,   _,   _  ) => { Opcode::ADD_IMMEDIATE { register_x: register_x, value: imm8 } }
      (0x8, _,   _,   0x0) => { Opcode::LD_REGISTER   { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0x1) => { Opcode::OR            { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0x2) => { Opcode::AND           { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0x3) => { Opcode::XOR           { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0x4) => { Opcode::ADD_REGISTER  { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0x5) => { Opcode::SUB           { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0x6) => { Opcode::SHR           { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0x7) => { Opcode::SUBN          { register_x: register_x, register_y: register_y } }
      (0x8, _,   _,   0xE) => { Opcode::SHL           { register_x: register_x, register_y: register_y } }
      (0x9, _,   _,   0x0) => { Opcode::SNE_REGISTER  { register_x: register_x, register_y: register_y } }
      (0xA, _,   _,   _  ) => { Opcode::LD_I          { value: imm12 } }
      (0xB, _,   _,   _  ) => { Opcode::JP_V0         { value: imm12 } }
      (0xC, _,   _,   _  ) => { Opcode::RND           { register_x: register_x, mask: imm8 } }
      (0xD, _,   _,   _  ) => { Opcode::DRW           { register_x: register_x, register_y: register_y, bytes: imm4 } }
      (0xE, _,   0x9, 0xE) => { Opcode::SE_K          { register_x: register_x } }
      (0xE, _,   0xA, 0x1) => { Opcode::SNE_K         { register_x: register_x } }
      (0xF, _,   0x0, 0x7) => { Opcode::LD_X_DT       { register_x: register_x } }
      (0xF, _,   0x0, 0xA) => { Opcode::LD_X_K        { register_x: register_x } }
      (0xF, _,   0x1, 0x5) => { Opcode::LD_DT_X       { register_x: register_x } }
      (0xF, _,   0x1, 0x8) => { Opcode::LD_ST_X       { register_x: register_x } }
      (0xF, _,   0x1, 0xE) => { Opcode::ADD_I_X       { register_x: register_x } }
      (0xF, _,   0x2, 0x9) => { Opcode::SPRITE_I      { register_x: register_x } }
      (0xF, _,   0x3, 0x3) => { Opcode::BCD_I         { register_x: register_x } }
      (0xF, _,   0x5, 0x5) => { Opcode::SAVE_I        { register_x: register_x } }
      (0xF, _,   0x6, 0x5) => { Opcode::RESTORE_I     { register_x: register_x } }
      (_,   _,   _,   _  ) => { Opcode::NOP           }
    }
  }

  pub fn to_assembly_string(&self) -> String {
    match *self {
      Opcode::NOP                                                     => { format!("nop") }
      Opcode::CLS                                                     => { format!("cls") }
      Opcode::RET                                                     => { format!("ret") }
      Opcode::TRAP                                                    => { format!("trap") }
      Opcode::TRAPRET                                                 => { format!("trapret") }
      Opcode::JP              { target }                              => { format!("jp ${:3x}", target.as_u16()) }
      Opcode::CALL            { target }                              => { format!("call ${:3x}", target.as_u16()) }
      Opcode::SE_IMMEDIATE    { register_x: x, value }                => { format!("se v{:1x}, ${:2x}", x, value) }
      Opcode::SNE_IMMEDIATE   { register_x: x, value }                => { format!("sne v{:1x}, ${:2x}", x, value) }
      Opcode::SE_REGISTER     { register_x: x, register_y: y }        => { format!("se v{:1x}, v{:1x}", x, y) }
      Opcode::SNE_REGISTER    { register_x: x, register_y: y }        => { format!("sne v{:1x}, v{:1x}", x, y) }
      Opcode::LD_IMMEDIATE    { register_x: x, value }                => { format!("ld v{:1x}, ${:2x}", x, value) }
      Opcode::ADD_IMMEDIATE   { register_x: x, value }                => { format!("add v{:1x}, ${:2x}", x, value) }
      Opcode::LD_REGISTER     { register_x: x, register_y: y }        => { format!("ld v{:1x}, v{:1x}", x, y) }
      Opcode::OR              { register_x: x, register_y: y }        => { format!("or v{:1x}, v{:1x}", x, y) }
      Opcode::AND             { register_x: x, register_y: y }        => { format!("and v{:1x}, v{:1x}", x, y) }
      Opcode::XOR             { register_x: x, register_y: y }        => { format!("xor v{:1x}, v{:1x}", x, y) }
      Opcode::ADD_REGISTER    { register_x: x, register_y: y }        => { format!("add v{:1x}, v{:1x}", x, y) }
      Opcode::SUB             { register_x: x, register_y: y }        => { format!("sub v{:1x}, v{:1x}", x, y) }
      Opcode::SHR             { register_x: x, register_y: y }        => { format!("shr v{:1x}, v{:1x}", x, y) }
      Opcode::SUBN            { register_x: x, register_y: y }        => { format!("subn v{:1x}, v{:1x}", x, y) }
      Opcode::SHL             { register_x: x, register_y: y }        => { format!("shl v{:1x}, v{:1x}", x, y) }
      Opcode::LD_I            { value }                               => { format!("ld i, ${:3x}", value.as_u16()) }
      Opcode::JP_V0           { value }                               => { format!("jp v0, ${:3x}", value.as_u16()) }
      Opcode::RND             { register_x: x, mask }                 => { format!("rnd v{:1x}, ${:2x}", x, mask) }
      Opcode::DRW             { register_x: x, register_y: y, bytes}  => { format!("drw v{:1x}, v{:1x}, ${:1x}", x, y, bytes) }
      Opcode::SE_K            { register_x: x }                       => { format!("se v{:1x}, k", x) }
      Opcode::SNE_K           { register_x: x }                       => { format!("sne v{:1x}, k", x) }
      Opcode::LD_X_DT         { register_x: x }                       => { format!("ld v{:1x}, dt", x) }
      Opcode::LD_X_K          { register_x: x }                       => { format!("ld v{:1x}, k", x) }
      Opcode::LD_DT_X         { register_x: x }                       => { format!("ld dt, v{:1x}", x) }
      Opcode::LD_ST_X         { register_x: x }                       => { format!("ld st, v{:1x}", x) }
      Opcode::ADD_I_X         { register_x: x }                       => { format!("add i, v{:1x}", x) }
      Opcode::SPRITE_I        { register_x: x }                       => { format!("sprite i, v{:1x}", x) }
      Opcode::BCD_I           { register_x: x }                       => { format!("bcd [i], v{:1x}", x) }
      Opcode::SAVE_I          { register_x: x }                       => { format!("save [i], v{:1x}", x) }
      Opcode::RESTORE_I       { register_x: x }                       => { format!("restore v{:1x}, [i]", x) }
    }
  }

  pub fn as_instruction(&self) -> u16 {
    match *self {
      Opcode::NOP                                                     => { 0x0000 }
      Opcode::CLS                                                     => { 0x00E0 }
      Opcode::RET                                                     => { 0x00EE }
      Opcode::TRAPRET                                                 => { 0x00F9 }
      Opcode::TRAP                                                    => { 0x00FA }
      Opcode::JP              { target }                              => { format_instruction_imm12(0x1, target) }
      Opcode::CALL            { target }                              => { format_instruction_imm12(0x2, target) }
      Opcode::SE_IMMEDIATE    { register_x: x, value }                => { format_instruction_x_imm8(0x3, x, value) }
      Opcode::SNE_IMMEDIATE   { register_x: x, value }                => { format_instruction_x_imm8(0x4, x, value) }
      Opcode::SE_REGISTER     { register_x: x, register_y: y }        => { format_instruction_x_y(0x5, x, y, 0x0) }
      Opcode::SNE_REGISTER    { register_x: x, register_y: y }        => { format_instruction_x_y(0x9, x, y, 0x0) }
      Opcode::LD_IMMEDIATE    { register_x: x, value }                => { format_instruction_x_imm8(0x6, x, value) }
      Opcode::ADD_IMMEDIATE   { register_x: x, value }                => { format_instruction_x_imm8(0x7, x, value) }
      Opcode::LD_REGISTER     { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x0) }
      Opcode::OR              { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x1) }
      Opcode::AND             { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x2) }
      Opcode::XOR             { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x3) }
      Opcode::ADD_REGISTER    { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x4) }
      Opcode::SUB             { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x5) }
      Opcode::SHR             { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x6) }
      Opcode::SUBN            { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0x7) }
      Opcode::SHL             { register_x: x, register_y: y }        => { format_instruction_x_y(0x8, x, y, 0xE) }
      Opcode::LD_I            { value }                               => { format_instruction_imm12(0xA, value) }
      Opcode::JP_V0           { value }                               => { format_instruction_imm12(0xB, value) }
      Opcode::RND             { register_x: x, mask }                 => { format_instruction_x_imm8(0xC, x, mask) }
      Opcode::DRW             { register_x: x, register_y: y, bytes}  => { format_instruction_x_y(0xD, x, y, bytes) }
      Opcode::SE_K            { register_x: x }                       => { format_instruction_x_imm8(0xE, x, 0x9E) }
      Opcode::SNE_K           { register_x: x }                       => { format_instruction_x_imm8(0xE, x, 0xA1) }
      Opcode::LD_X_DT         { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x07) }
      Opcode::LD_X_K          { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x0A) }
      Opcode::LD_DT_X         { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x15) }
      Opcode::LD_ST_X         { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x18) }
      Opcode::ADD_I_X         { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x1E) }
      Opcode::SPRITE_I        { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x29) }
      Opcode::BCD_I           { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x33) }
      Opcode::SAVE_I          { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x55) }
      Opcode::RESTORE_I       { register_x: x }                       => { format_instruction_x_imm8(0xF, x, 0x65) }
    }
  }

  pub fn to_formatted_assembly_string(&self, leading_spaces: u8) -> String {
    (0..leading_spaces).map(|_| " ").collect::<String>() + &self.to_string()
  }

}

impl fmt::Display for Opcode {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.to_assembly_string())
  }
}

fn format_instruction_imm12(hi4: u8, imm12: U12) -> u16 {
  /*
   * Instruction format A consists of a 4-bit prefix and a 12-bit immediate.
   * +------+----------------+
   * | hi-4 | nnnn nnnn nnnn |
   * +------+----------------+
   */
  assert!(  hi4 <= 0xF);

  ((hi4 as u16) << 12) | (imm12.as_u16())
}

fn format_instruction_x_imm8(hi4: u8, x: u8, imm8: u8) -> u16 {
  /*
   * Instruction format B consists of a 4-bit prefix, 4-bit register (x) and 8-bit immediate.
   * +------+------+-----------+
   * | hi-4 | -rx- | nnnn nnnn |
   * +------+------+-----------+
   */
  assert!(hi4 <= 0xF);
  assert!(  x <= 0xF);

  ((hi4 as u16) << 12) | ((x as u16) << 8) | (imm8 as u16)
}

fn format_instruction_x_y(hi4: u8, x: u8, y: u8, lo4: u8) -> u16 {
  /*
   * Instruction format C consists of a 4-bit prefix, 4-bit x and y and a 4-bit suffix.
   * +------+------+------+------+
   * | hi-4 | -rx- | -ry- | nnnn |
   * +------+------+------+------+
   */
  assert!(hi4 <= 0xF);
  assert!(  x <= 0xF);
  assert!(  y <= 0xF);
  assert!(lo4 <= 0xF);

  ((hi4 as u16) << 12) | ((x as u16) << 8) | ((y as u16) << 4) | (lo4 as u16)
}

#[cfg(test)]
mod tests {

  use super::*;

  #[test]
  fn test_from_instruction_round_trips() {
    // Round trip all possible instructions.
    for i in 0..0xFFFFu16 {
      let opcode = Opcode::from_instruction(i);
      let instruction_from_opcode = opcode.as_instruction();
      let round_trip_opcode = Opcode::from_instruction(instruction_from_opcode);

      assert!(opcode == round_trip_opcode, "Unable to round-trip instruction 0x{:04x}", i);

      match opcode {
        Opcode::NOP => {
          /* Many different instructions map to NOP. */
        }
        _ => {
          assert!(i == round_trip_opcode.as_instruction(),
            "Unable to round trip {:04X}; result was {:04X} ({})", 
            i, round_trip_opcode.as_instruction(), round_trip_opcode);
        }
      }
    }
  }

  #[test]
  fn test_to_assembly_string() {
    let test_instructions: [u16; 37] = [ 
      /* NOP          */ 0x0000,
      /* CLS          */ 0x00E0,
      /* RET          */ 0x00EE,
      /* TRAPRET      */ 0x00F9,
      /* TRAP         */ 0x00FA,
      /* JP $456      */ 0x1456,
      /* CALL $789    */ 0x2789,
      /* SE V1, $DE   */ 0x31DE,
      /* SNE V2, $EF  */ 0x42EF,
      /* SE V5, V6    */ 0x5560,
      /* LD V1, $55   */ 0x6155,
      /* ADD V2, $66  */ 0x7266,
      /* LD V3, V4    */ 0x8340,
      /* OR V5, V6    */ 0x8561,
      /* AND V5, V6   */ 0x8562,
      /* XOR V5, V6   */ 0x8563,
      /* ADD V5, V6   */ 0x8564,
      /* SUB V5, V6   */ 0x8565,
      /* SHR V5, V6   */ 0x8566,
      /* SUBN V5, V6  */ 0x8567,
      /* SHL V5, V6   */ 0x856E,
      /* SNE VE, VF   */ 0x9EF0,
      /* LD I, $DEF   */ 0xADEF,
      /* JP V0, $234  */ 0xB234,
      /* RND V1, $8F  */ 0xC18F,
      /* DRW V2,V3,$4 */ 0xD234,
      /* SE V1, K     */ 0xE19E,
      /* SNE V1, K    */ 0xE1A1,
      /* LD V2, DT    */ 0xF207,
      /* LD V3, K     */ 0xF30A,
      /* LD DT, V3    */ 0xF315,
      /* LD ST, V3    */ 0xF318,
      /* ADD I, V5    */ 0xF51E,
      /* SPRITE V6    */ 0xF629,
      /* BCD [I], V7  */ 0xF733,
      /* SAVE [I], VE */ 0xFE55,
      /* RSTR VF, [I] */ 0xFF65
    ];

    let test_assembly_strings: [&'static str; 37] = [
      "nop",
      "cls",
      "ret",
      "trapret",
      "trap",
      "jp $456",
      "call $789",
      "se v1, $de",
      "sne v2, $ef",
      "se v5, v6",
      "ld v1, $55",
      "add v2, $66",
      "ld v3, v4",
      "or v5, v6",
      "and v5, v6",
      "xor v5, v6",
      "add v5, v6",
      "sub v5, v6",
      "shr v5, v6",
      "subn v5, v6",
      "shl v5, v6",
      "sne ve, vf",
      "ld i, $def",
      "jp v0, $234",
      "rnd v1, $8f",
      "drw v2, v3, $4",
      "se v1, k",
      "sne v1, k",
      "ld v2, dt",
      "ld v3, k",
      "ld dt, v3",
      "ld st, v3",
      "add i, v5",
      "sprite i, v6",
      "bcd [i], v7",
      "save [i], ve",
      "restore vf, [i]"
    ];

    // Validate opcodes decoded from the test set yield expected assembly strings.
    for i in 0 .. test_instructions.iter().count() {
      let instruction = test_instructions[i];
      let assembly_string = test_assembly_strings[i].to_string();
      let opcode = Opcode::from_instruction(instruction);
      assert_eq!(opcode.to_assembly_string(), assembly_string);
    }
  }

}
