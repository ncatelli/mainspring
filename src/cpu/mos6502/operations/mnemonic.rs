extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{ParseResult, Parser};

// Load-Store
#[derive(Clone, Copy)]
pub struct LDA;

#[derive(Clone, Copy)]
pub struct LDX;

#[derive(Clone, Copy)]
pub struct LDY;

#[derive(Clone, Copy)]
pub struct STA;

#[derive(Clone, Copy)]
pub struct STX;

#[derive(Clone, Copy)]
pub struct STY;

// Arithmetic
#[derive(Clone, Copy)]
pub struct ADC;

#[derive(Clone, Copy)]
pub struct SBC;

#[derive(Clone, Copy)]
pub struct INC;

#[derive(Clone, Copy)]
pub struct INX;

#[derive(Clone, Copy)]
pub struct INY;

#[derive(Clone, Copy)]
pub struct DEC;

#[derive(Clone, Copy)]
pub struct DEX;

#[derive(Clone, Copy)]
pub struct DEY;

// Shift and Rotate
#[derive(Clone, Copy)]
pub struct ASL;

#[derive(Clone, Copy)]
pub struct LSR;

#[derive(Clone, Copy)]
pub struct ROL;

#[derive(Clone, Copy)]
pub struct ROR;

#[derive(Clone, Copy)]
pub struct AND;

#[derive(Clone, Copy)]
pub struct ORA;

#[derive(Clone, Copy)]
pub struct EOR;

// Compare and Test Bit
#[derive(Clone, Copy)]
pub struct CMP;

#[derive(Clone, Copy)]
pub struct CPX;

#[derive(Clone, Copy)]
pub struct CPY;

#[derive(Clone, Copy)]
pub struct BIT;

// Branch
#[derive(Clone, Copy)]
pub struct BCC;

#[derive(Clone, Copy)]
pub struct BCS;

#[derive(Clone, Copy)]
pub struct BNE;

#[derive(Clone, Copy)]
pub struct BEQ;

#[derive(Clone, Copy)]
pub struct BPL;

#[derive(Clone, Copy)]
pub struct BMI;

#[derive(Clone, Copy)]
pub struct BVC;

#[derive(Clone, Copy)]
pub struct BVS;

// Transfer
#[derive(Clone, Copy)]
pub struct TAX;

#[derive(Clone, Copy)]
pub struct TXA;

#[derive(Clone, Copy)]
pub struct TAY;

#[derive(Clone, Copy)]
pub struct TYA;

#[derive(Clone, Copy)]
pub struct TSX;

#[derive(Clone, Copy)]
pub struct TXS;

// Stack Operations
#[derive(Clone, Copy)]
pub struct PHA;

#[derive(Clone, Copy)]
pub struct PLA;

#[derive(Clone, Copy)]
pub struct PHP;

#[derive(Clone, Copy)]
pub struct PLP;

// Subroutines and Jump
#[derive(Clone, Copy)]
pub struct JMP;

#[derive(Clone, Copy)]
pub struct JSR;

#[derive(Clone, Copy)]
pub struct RTS;

#[derive(Clone, Copy)]
pub struct RTI;

// Set and Clear
#[derive(Clone, Copy)]
pub struct CLC;

#[derive(Clone, Copy)]
pub struct SEC;

#[derive(Clone, Copy)]
pub struct CLD;

#[derive(Clone, Copy)]
pub struct SED;

#[derive(Clone, Copy)]
pub struct CLI;

#[derive(Clone, Copy)]
pub struct SEI;

#[derive(Clone, Copy)]
pub struct CLV;

// Misc
#[derive(Clone, Copy)]
pub struct BRK;

/// Represents a `nop` instruction, only implemented for the implied address
/// mode and functions as a "No Operation".
#[derive(Clone, Copy)]
pub struct NOP;

impl Cyclable for NOP {}
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
