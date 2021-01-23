extern crate parcel;
use crate::cpu::Offset;
use parcel::{ParseResult, Parser};

// Load-Store
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LDA;

impl Offset for LDA {}

impl<'a> Parser<'a, &'a [u8], LDA> for LDA {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], LDA> {
        parcel::one_of(vec![
            parcel::parsers::byte::expect_byte(0xa9),
            parcel::parsers::byte::expect_byte(0xa5),
            parcel::parsers::byte::expect_byte(0xb5),
            parcel::parsers::byte::expect_byte(0xad),
        ])
        .map(|_| LDA)
        .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LDX;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LDY;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct STA;

impl Offset for STA {}

impl<'a> Parser<'a, &'a [u8], STA> for STA {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], STA> {
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0x8d)])
            .map(|_| STA)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct STX;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct STY;

// Arithmetic
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ADC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SBC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct INC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct INX;

impl Offset for INX {}

impl<'a> Parser<'a, &'a [u8], INX> for INX {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], INX> {
        parcel::parsers::byte::expect_byte(0xe8)
            .map(|_| INX)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct INY;

impl Offset for INY {}

impl<'a> Parser<'a, &'a [u8], INY> for INY {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], INY> {
        parcel::parsers::byte::expect_byte(0xc8)
            .map(|_| INY)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct DEC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct DEX;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct DEY;

// Shift and Rotate
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ASL;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LSR;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ROL;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ROR;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct AND;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ORA;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct EOR;

// Compare and Test Bit
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CMP;

impl Offset for CMP {}

impl<'a> Parser<'a, &'a [u8], CMP> for CMP {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], CMP> {
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0xc9)])
            .map(|_| CMP)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CPX;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CPY;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BIT;

// Branch
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BCC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BCS;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BNE;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BEQ;

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

impl Offset for TAX {}

impl<'a> Parser<'a, &'a [u8], TAX> for TAX {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TAX> {
        parcel::parsers::byte::expect_byte(0xaa)
            .map(|_| TAX)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TXA;

impl Offset for TXA {}

impl<'a> Parser<'a, &'a [u8], TXA> for TXA {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TXA> {
        parcel::parsers::byte::expect_byte(0x8a)
            .map(|_| TXA)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TAY;

impl Offset for TAY {}

impl<'a> Parser<'a, &'a [u8], TAY> for TAY {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TAY> {
        parcel::parsers::byte::expect_byte(0xa8)
            .map(|_| TAY)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TYA;

impl Offset for TYA {}

impl<'a> Parser<'a, &'a [u8], TYA> for TYA {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TYA> {
        parcel::parsers::byte::expect_byte(0x98)
            .map(|_| TYA)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TSX;

impl Offset for TSX {}

impl<'a> Parser<'a, &'a [u8], TSX> for TSX {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TSX> {
        parcel::parsers::byte::expect_byte(0xba)
            .map(|_| TSX)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TXS;

impl Offset for TXS {}

impl<'a> Parser<'a, &'a [u8], TXS> for TXS {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TXS> {
        parcel::parsers::byte::expect_byte(0x9a)
            .map(|_| TXS)
            .parse(input)
    }
}

// Stack Operations
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PHA;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PLA;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PHP;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct PLP;

// Subroutines and Jump

/// Represents a `jmp` instruction, only implemented for the absolute address
/// and indirect modes and functions as jump to a location in memory.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct JMP;

impl Offset for JMP {}

impl<'a> Parser<'a, &'a [u8], JMP> for JMP {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], JMP> {
        parcel::one_of(vec![
            parcel::parsers::byte::expect_byte(0x4c),
            parcel::parsers::byte::expect_byte(0x6c),
        ])
        .map(|_| JMP)
        .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct JSR;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct RTS;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct RTI;

// Set and Clear
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLC;

impl Offset for CLC {}

impl<'a> Parser<'a, &'a [u8], CLC> for CLC {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], CLC> {
        parcel::parsers::byte::expect_byte(0xad)
            .map(|_| CLC)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SEC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLD;

impl Offset for CLD {}

impl<'a> Parser<'a, &'a [u8], CLD> for CLD {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], CLD> {
        parcel::parsers::byte::expect_byte(0xd8)
            .map(|_| CLD)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SED;

impl Offset for SED {}

impl<'a> Parser<'a, &'a [u8], SED> for SED {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], SED> {
        parcel::parsers::byte::expect_byte(0xf8)
            .map(|_| SED)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLI;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SEI;

impl Offset for SEI {}

impl<'a> Parser<'a, &'a [u8], SEI> for SEI {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], SEI> {
        parcel::parsers::byte::expect_byte(0x78)
            .map(|_| SEI)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLV;

// Misc
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct BRK;

/// Represents a `nop` instruction, only implemented for the implied address
/// mode and functions as a "No Instruction".
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct NOP;

impl Offset for NOP {}

impl<'a> Parser<'a, &'a [u8], NOP> for NOP {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], NOP> {
        parcel::parsers::byte::expect_byte(0xea)
            .map(|_| NOP)
            .parse(input)
    }
}
