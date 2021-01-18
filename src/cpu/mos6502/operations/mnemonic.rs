extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{ParseResult, Parser};

// Load-Store
#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LDX;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LDY;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct STA;

impl Offset for STA {}

impl<'a> Parser<'a, &'a [u8], STA> for STA {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], STA> {
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0x8d)])
            .map(|_| STA)
            .parse(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct STX;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct STY;

// Arithmetic
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ADC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SBC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct INC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct INX;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct INY;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DEC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DEX;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DEY;

// Shift and Rotate
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ASL;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LSR;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ROL;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ROR;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AND;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ORA;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EOR;

// Compare and Test Bit
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CMP;

impl Offset for CMP {}

impl<'a> Parser<'a, &'a [u8], CMP> for CMP {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], CMP> {
        parcel::one_of(vec![parcel::parsers::byte::expect_byte(0xc9)])
            .map(|_| CMP)
            .parse(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CPX;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CPY;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BIT;

// Branch
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BCC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BCS;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BNE;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BEQ;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BPL;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BMI;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BVC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BVS;

// Transfer
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TAX;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TXA;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TAY;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TYA;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TSX;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TXS;

// Stack Operations
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PHA;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PLA;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PHP;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PLP;

// Subroutines and Jump

/// Represents a `jmp` instruction, only implemented for the absolute address
/// and indirect modes and functions as jump to a location in memory.
#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct JSR;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RTS;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RTI;

// Set and Clear
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CLC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SEC;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CLD;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SED;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CLI;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SEI;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CLV;

// Misc
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BRK;

/// Represents a `nop` instruction, only implemented for the implied address
/// mode and functions as a "No Instruction".
#[derive(Debug, Clone, Copy, PartialEq)]
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
