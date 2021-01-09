extern crate parcel;
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
pub struct NOP;

impl<'a> Parser<'a, &'a [u8], NOP> for NOP {
    fn parse(&self, _: &'a [u8]) -> ParseResult<&'a [u8], NOP> {
        todo!()
    }
}

/// Address modes
pub struct Accumulator;
pub struct Implied;
pub struct Immediate(u8);
pub struct Absolute(u16);
pub struct ZeroPage(u8);
pub struct Relative(i8);
pub struct Indirect(u16);
pub struct AbsoluteIndexedWithX(u16);
pub struct AbsoluteIndexedWithY(u16);
pub struct ZeroPageIndexedWithX(u8);
pub struct ZeroPageIndexedWithY(u8);
pub struct IndexedIndirect(u8);
pub struct IndirectIndexed(u8);

#[derive(Copy, Clone, PartialEq, Debug)]
/// Operation takes a mnemonic
pub struct Operation<M, A> {
    mnemonic: M,
    address_mode: A,
}
