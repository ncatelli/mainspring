extern crate parcel;
use crate::cpu::Offset;

macro_rules! generate_mnemonic_parser_and_offset {
    ($mnemonic:ty, $opcode:literal) => {
        impl Offset for $mnemonic {}

        impl<'a> parcel::Parser<'a, &'a [u8], $mnemonic> for $mnemonic {
            fn parse(&self, input: &'a [u8]) -> parcel::ParseResult<&'a [u8], $mnemonic> {
                parcel::parsers::byte::expect_byte($opcode)
                    .map(|_| <$mnemonic>::default())
                    .parse(input)
            }
        }
    };

    ($mnemonic:ty, $( $opcode:literal ),* ) => {
        impl Offset for $mnemonic {}

        impl<'a> parcel::Parser<'a, &'a [u8], $mnemonic> for $mnemonic {
            fn parse(&self, input: &'a [u8]) -> parcel::ParseResult<&'a [u8], $mnemonic> {
                parcel::one_of(vec![
                    $(
                        parcel::parsers::byte::expect_byte($opcode),
                    )*
                ])
                    .map(|_| <$mnemonic>::default())
                    .parse(input)
            }
        }
    };
}

/// Load operand into Accumulator
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LDA;

generate_mnemonic_parser_and_offset!(LDA, 0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1);

/// Load operand into X Register
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LDX;

generate_mnemonic_parser_and_offset!(LDX, 0xa2, 0xa6, 0xb6, 0xae, 0xbe);

/// Load operand into Y Register
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LDY;

generate_mnemonic_parser_and_offset!(LDY, 0xa0, 0xa4, 0xb4, 0xac, 0xbc);

// Store Accumulator in memory
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct STA;

generate_mnemonic_parser_and_offset!(STA, 0x8d, 0x85, 0x95, 0x9d, 0x99, 0x81, 0x91);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct STX;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct STY;

// Arithmetic
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ADC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SBC;

/// Increment Memory by one.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct INC;

generate_mnemonic_parser_and_offset!(INC, 0xee, 0xfe, 0xe6, 0xf6);

/// Increment X register by one.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct INX;

generate_mnemonic_parser_and_offset!(INX, 0xe8);

/// Increment Y register by one.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct INY;

generate_mnemonic_parser_and_offset!(INY, 0xc8);

/// Decrement memory by one.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct DEC;

generate_mnemonic_parser_and_offset!(DEC, 0xce);

/// Decrement X register by one.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct DEX;

generate_mnemonic_parser_and_offset!(DEX, 0xca);

/// Decrement Y register by one.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct DEY;

generate_mnemonic_parser_and_offset!(DEY, 0x88);

// Shift and Rotate
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ASL;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LSR;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ROL;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ROR;

/// And Memory with Accumulator.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct AND;

generate_mnemonic_parser_and_offset!(AND, 0x2d, 0x3d, 0x39, 0x21, 0x29, 0x31, 0x25, 0x35);

/// Or memory with Accumulator.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ORA;

generate_mnemonic_parser_and_offset!(ORA, 0x0d, 0x1d, 0x19, 0x11, 0x09, 0x01, 0x05, 0x15);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct EOR;

// Compare and Test Bit

/// Compare memory with Accumulator
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CMP;

generate_mnemonic_parser_and_offset!(CMP, 0xc9, 0xcd, 0xc5, 0xd5, 0xdd, 0xd9, 0xc1, 0xd1);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CPX;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CPY;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BIT;

// Branch
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BCC;

generate_mnemonic_parser_and_offset!(BCC, 0x90);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BCS;

generate_mnemonic_parser_and_offset!(BCS, 0xb0);

/// Branch on Zero. Follows branch when the Zero flag is not set.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BNE;

generate_mnemonic_parser_and_offset!(BNE, 0xd0);

/// Branch on Zero. Follows branch when the Zero flag is set.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BEQ;

generate_mnemonic_parser_and_offset!(BEQ, 0xf0);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BPL;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BMI;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BVC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BVS;

// Transfer
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TAX;

generate_mnemonic_parser_and_offset!(TAX, 0xaa);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TXA;

generate_mnemonic_parser_and_offset!(TXA, 0x8a);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TAY;

generate_mnemonic_parser_and_offset!(TAY, 0xa8);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TYA;

generate_mnemonic_parser_and_offset!(TYA, 0x98);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TSX;

generate_mnemonic_parser_and_offset!(TSX, 0xba);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TXS;

generate_mnemonic_parser_and_offset!(TXS, 0x9a);

// Stack Operations

/// Push Accumulator on stack
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PHA;

generate_mnemonic_parser_and_offset!(PHA, 0x48);

/// Pull Accumulator from stack.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PLA;

generate_mnemonic_parser_and_offset!(PLA, 0x46);

/// Push Processor Status to stack.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PHP;

generate_mnemonic_parser_and_offset!(PHP, 0x08);

// Pull Processor Status from stack.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PLP;

generate_mnemonic_parser_and_offset!(PLP, 0x28);

// Subroutines and Jump

/// Represents a `jmp` instruction, only implemented for the absolute address
/// and indirect modes and functions as jump to a location in memory.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct JMP;

generate_mnemonic_parser_and_offset!(JMP, 0x4c, 0x6c);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct JSR;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct RTS;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct RTI;

// Set and Clear
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLC;

generate_mnemonic_parser_and_offset!(CLC, 0xad);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SEC;

generate_mnemonic_parser_and_offset!(SEC, 0x38);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLD;

generate_mnemonic_parser_and_offset!(CLD, 0xd8);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SED;

generate_mnemonic_parser_and_offset!(SED, 0xf8);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLI;

generate_mnemonic_parser_and_offset!(CLI, 0x58);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SEI;

generate_mnemonic_parser_and_offset!(SEI, 0x78);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLV;

generate_mnemonic_parser_and_offset!(CLV, 0xb8);

// Misc
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BRK;

/// Represents a `nop` instruction, only implemented for the implied address
/// mode and functions as a "No Instruction".
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct NOP;

generate_mnemonic_parser_and_offset!(NOP, 0xea);
