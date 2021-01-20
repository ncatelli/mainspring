extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{ParseResult, Parser};

// Load-Store
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct LDA;

impl Cyclable for LDA {}
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

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct INY;

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
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0xaa)])
            .map(|_| TAX)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TXA;

impl Offset for TXA {}

impl<'a> Parser<'a, &'a [u8], TXA> for TXA {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TXA> {
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0x8a)])
            .map(|_| TXA)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TAY;

impl Offset for TAY {}

impl<'a> Parser<'a, &'a [u8], TAY> for TAY {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TAY> {
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0xa8)])
            .map(|_| TAY)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TYA;

impl Offset for TYA {}

impl<'a> Parser<'a, &'a [u8], TYA> for TYA {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], TYA> {
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0x98)])
            .map(|_| TYA)
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TSX;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TXS;

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

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SEC;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLD;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SED;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CLI;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SEI;

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

impl From<NOP> for u8 {
    fn from(_: NOP) -> Self {
        0xea
    }
}

impl<'a> Parser<'a, &'a [u8], NOP> for NOP {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], NOP> {
        parcel::parsers::byte::expect_byte(NOP.into())
            .map(|_| NOP)
            .parse(input)
    }
}
