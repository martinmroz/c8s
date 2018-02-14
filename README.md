c8s
===

Chip-8/48 Assembler written in Rust. Written originally to support the `FPC8` FPGA Chip-8 implementation I've been working on in my spare time. It supports the common dialect of Chip-8/48 as defined in the [Cowgod Chip-8 Technical Reference v1.0](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM) in addition to some `FPC8` specific extensions. The output will be compatible with any Chip-8/48 interpreter.

[![Build Status](https://travis-ci.org/martinmroz/c8s.svg?branch=master)](https://travis-ci.org/martinmroz/c8s)
[![Coverage Status](https://coveralls.io/repos/github/martinmroz/c8s/badge.svg?branch=master)](https://coveralls.io/github/martinmroz/c8s?branch=master)

## Usage

```
USAGE: target/debug/c8s [options] <input>

Options:
    -o, --output <file> Write output to <file>
    -h, --help          Display available options
    -v, --version       Display version information
```

### Output

The default output file name, if not specified, is `a.hex`. If you'd like to output the result to `STDOUT` you can use `-o -`.

The file format of the output is 8-bit Intel HEX [`I8HEX`](https://en.wikipedia.org/wiki/Intel_HEX). This file format is commonly used in embedded systems as it supports multiple disjoint address ranges in addition to checksumming each chunk to improve reliability of transfers.

## Literals
The following literal types are supported.

* 4-bit numeric.
	* Valid in the context of a mnemonic or within directives.
	* Binary `%0000...%1111`
	* Decimal `0...15`
	* Hexadecimal `$0-$F` and `#0-#F` (case-insensitive).
	* A label in the range `$0-$F` whether constant or address-based.
* 8-bit numeric.
	* Valid in the context of a mnemonic or within directives.
	* Binary `%00000000...%11111111`
	* Decimal `0...255`
	* Hexadecimal `$0-$FF` and `#0-#FF` (case-insensitive).
	* A label in the range `$00-$FF` whether constant or address-based.
* 12-bit numeric.
	* Valid in the context of a mnemonic only, not within directives.
	* Binary `%000000000000...%111111111111`
	* Decimal `0...4095`
	* Hexadecimal `$0-$FFF` and `#0-#FFF` (case-insensitive).
	* A label in the range `$000-$FFF` whether constant or address-based.
* String.
	* Valid only in the context of `.DB` directive.
	* NOT null-terminated.

## Directives
`c8s` supports the following directives, the mnemonics for which are not case sensitive.

* `.DB <args>`
	* Directly emit all literals that follow (comma-separated).
		* 8-bit numeric: Directly emitted.
		* String: Directly emitted without a trailing `\0` and escape characters are currently unsupported. To incorporate an unsupported character, end the string and add an 8-bit hexadecimal literal. Unicode characters are supported.
* `.ORG addr`
	* Sets the new output origin to `addr`. 
	* Origin directives do not have to be increasing. The origin may be set to something below the current address counter. No two pieces of data may be written to the same address, and this will trigger an error. NOTE: The `c8s` assembler begins at origin `$000` and you must utilize the Origin directive to change that.
* `.EQU <value>`
	* Sets the value of the immediately-preceding label to `value`.

## Labels

Any identifier followed by a colon (`NAME:`) is considered a label. Labels are case-sensitive when referenced from elsewhere in the assembly. As a two-pass assembler, labels do not need to be defined in advance of being used. Labels may be defined as constants
through through the `.EQU` directive.

## Mnemonics

### Standard Set
`c8s` supports effectively all standard Chip-8/48 mnemonics (not Super Chip-48). The mnemonic set supported is compatbile with the [Cowgod Chip-8 Technical Reference v1.0](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM). The mnemonics and registers are case-insensitive. Any literal value can be specified in place or via label.

| Opcode | Mnemonic        | Function                                                    |
| ------ | --------------- | ----------------------------------------------------------- |
| 00E0   | `CLS`           | Clear the display.                                          |
| 00EE   | `RET`           | Return from subroutine.                                     |
| 1nnn   | `JP addr`       | Jump to the address `nnn`.                                  |
| 2nnn   | `CALL addr`     | Call subroutine at address `nnn`.                           |
| 3xkk   | `SE Vx, nn`     | Skip the next instruction if `Vx == nn`.                    |
| 4xkk   | `SNE Vx, nn`    | Skip the next instruction if `Vx != nn`.                    |
| 5xy0   | `SE Vx, Vy`     | Skip the next instruction if `Vx == Vy`.                    |
| 6xkk   | `LD Vx, nn`     | Set register `Vx` to `nn`.                                  |
| 7xkk   | `ADD Vx, nn`    | Set register `Vx` to `Vx + nn`.                             |
| 8xy0   | `LD Vx, Vy`     | Set register `Vx` to `Vy`.                                  |
| 8xy1   | `OR Vx, Vy`     | Set register `Vx` to `Vx | Vy`.                             |
| 8xy2   | `AND Vx, Vy`    | Set register `Vx` to `Vx & Vy`.                             |
| 8xy3   | `XOR Vx, Vy`    | Set register `Vx` to `Vx ^ Vy`.                             |
| 8xy4   | `ADD Vx, Vy`    | Set register `Vx` to `Vx & Vy`, and `VF` to `Carry`.        |
| 8xy5   | `SUB Vx, Vy`    | Set register `Vx` to `Vx - Vy`, and `VF` to `!Borrow`.      |
| 8xy6   | `SHR Vx`        | Set register `Vx` to `Vx >> 1`, and `VF` to `LSB(Vx)`.      |
| 8xy7   | `SUBN Vx, Vy`   | Set register `Vx` to `Vy - Vx`, and `VF` to `!Borrow`.      |
| 8xyE   | `SHL Vx`        | Set register `Vx` to `Vx << 1`, and `VF` to `MSB(Vx)`.      |
| 9xy0   | `SNE Vx, Vy`    | Skip the next instruction if `Vx != Vy`.                    |
| Annn   | `LD I, addr`    | Set register `I` to address `nnn`.                          |
| Bnnn   | `JP V0, addr`   | Jump to the address `nnn + V0`.                             |
| Cxkk   | `RND Vx, nn`    | Set register `Vx` to `RAND & nn`, and `VF` to `COLLISION`.  |
| Dxyn   | `DRW Vx, Vy, n` | Draw `n` byte sprite from address `I` at `(Vx, Vy)`.        |
| Ex9E   | `SKP Vx`        | Skip next instruction if key value `Vx` is pressed.         |
| ExA1   | `SKNP Vx`       | Skip next instruction if key value `Vx` is not pressed.     |
| Fx07   | `LD Vx, DT`     | Set register `Vx` to the value of the delay timer.          |
| Fx0A   | `LD Vx, K`      | Wait for a key press and store the value in `Vx`.           |
| Fx15   | `LD DT, Vx`     | Set the delay timer to `Vx`.                                |
| Fx18   | `LD ST, Vx`     | Set the sound timer to `Vx`.                                |
| Fx1E   | `ADD I, Vx`     | Set register `I` to `I + Vx`.                               |
| Fx29   | `LD F, Vx`      | Set register `I` to `SPRITE_ADDRESS_OF(Vx)`.                |
| Fx33   | `LD B, Vx`      | Store `BCD(Vx)` into `[I ... I+2]`.                         |
| Fx55   | `LD [I], Vx`    | Store `V0 ... Vx` into `[I ... I+x]`.                       |
| Fx65   | `LD Vx, [I]`    | Load registers `V0 ... Vx` from `[I ... I+x]`.              |

### Extended Set for FPC8

The `c8s` assembler was written in support of the `FPC8` FPGA Chip-8 implementation. The following instructions are only valid on the `FPC8` and should not be used. No warning is issued.

| Opcode | Mnemonic        | Function                                                    |
| ------ | --------------- | ----------------------------------------------------------- |
| 00F8   | `LD I, [I]`     | Loads register `I` from the address stored in register `I`. |
| 00F9   | `TRAPRET`       | Return from a software trap.                                |
| 00FA   | `TRAP`          | Triggers a non-specific software trap.                      |

## Cargo Dependencies

* [`twelve_bit`](https://crates.io/crates/twelve_bit) A Rust library for representing 12-bit unsigned values. This is primarily useful for implementing Chip-8 assemblers and interpreters safely. The type implements the bulk of the standard Rust literal semantics and operators, and much of the documentation is adapted from the `u16` intrinsic type.
* [`ihex`](https://crates.io/crates/ihex) A Rust library for parsing and generating Intel HEX (or IHEX) objects. This format is commonly used for representing compiled program code and data to be loaded into a microcontroller, flash memory or ROM.

## License
Copyright 2016 Martin Mroz.

Licensed under the MIT License (the "License"); you may not use this software except in compliance with the License. You may obtain a copy of the License at

    https://opensource.org/licenses/MIT

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
