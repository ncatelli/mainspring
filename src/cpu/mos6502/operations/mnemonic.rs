extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{ParseResult, Parser};

// Load-Store
pub struct LDA;
pub struct LDX;
pub struct LDY;
pub struct STA;
pub struct STX;
pub struct STY;

// Arithmetic
pub struct ADC;
pub struct SBC;
pub struct INC;
pub struct INX;
pub struct INY;
pub struct DEC;
pub struct DEX;
pub struct DEY;

// Shift and Rotate
pub struct ASL;
pub struct LSR;
pub struct ROL;
pub struct ROR;
pub struct AND;
pub struct ORA;
pub struct EOR;

// Compare and Test Bit
pub struct CMP;
pub struct CPX;
pub struct CPY;
pub struct BIT;

// Branch
pub struct BCC;
pub struct BCS;
pub struct BNE;
pub struct BEQ;
pub struct BPL;
pub struct BMI;
pub struct BVC;
pub struct BVS;

// Transfer
pub struct TAX;
pub struct TXA;
pub struct TAY;
pub struct TYA;
pub struct TSX;
pub struct TXS;

// Stack Operations
pub struct PHA;
pub struct PLA;
pub struct PHP;
pub struct PLP;

// Subroutines and Jump
pub struct JMP;
pub struct JSR;
pub struct RTS;
pub struct RTI;

// Set and Clear
pub struct CLC;
pub struct SEC;
pub struct CLD;
pub struct SED;
pub struct CLI;
pub struct SEI;
pub struct CLV;

// Misc
pub struct BRK;

/// Represents a `nop` instruction, only implemented for the implied address
/// mode and functions as a "No Operation".
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
