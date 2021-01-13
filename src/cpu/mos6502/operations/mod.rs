extern crate parcel;
use crate::cpu::{
    mos6502::{Execute, MOS6502},
    Cyclable, Offset,
};
use parcel::{parsers::byte::expect_byte, ParseResult, Parser};
use std::fmt::Debug;

pub mod address_mode;
pub mod mnemonic;

#[cfg(test)]
mod tests;

pub struct Operation();

/// Instruction takes a mnemonic and address mode as arguments for sizing
/// and operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    mnemonic: M,
    address_mode: A,
}

impl<M, A> Instruction<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    pub fn new(mnemonic: M, address_mode: A) -> Self {
        Instruction {
            mnemonic,
            address_mode,
        }
    }
}

impl<M, A> Cyclable for Instruction<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    fn cycles(&self) -> usize {
        self.mnemonic.cycles() + self.address_mode.cycles()
    }
}

impl<M, A> Offset for Instruction<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    fn offset(&self) -> usize {
        self.mnemonic.offset() + self.address_mode.offset()
    }
}

/// LDA

impl std::convert::TryFrom<&[u8; 3]> for Instruction<mnemonic::LDA, address_mode::Immediate> {
    type Error = String;
    fn try_from(values: &[u8; 3]) -> std::result::Result<Self, Self::Error> {
        match Instruction::new(mnemonic::LDA, address_mode::Immediate::default()).parse(values) {
            Ok(parcel::MatchStatus::Match((_, op))) => Ok(op),
            _ => Err(format!("No match found for {}", values[0])),
        }
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::LDA, address_mode::Immediate>>
    for Instruction<mnemonic::LDA, address_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::LDA, address_mode::Immediate>> {
        expect_byte(0xa9)
            .and_then(|_| address_mode::Immediate::default())
            .map(|am| Instruction::new(mnemonic::LDA, am))
            .parse(input)
    }
}

/// STA

impl std::convert::TryFrom<&[u8; 3]> for Instruction<mnemonic::STA, address_mode::Absolute> {
    type Error = String;
    fn try_from(values: &[u8; 3]) -> std::result::Result<Self, Self::Error> {
        match Instruction::new(mnemonic::STA, address_mode::Absolute::default()).parse(values) {
            Ok(parcel::MatchStatus::Match((_, op))) => Ok(op),
            _ => Err(format!("No match found for {}", values[0])),
        }
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::STA, address_mode::Absolute>>
    for Instruction<mnemonic::STA, address_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::STA, address_mode::Absolute>> {
        expect_byte(0x8d)
            .and_then(|_| address_mode::Absolute::default())
            .map(|am| Instruction::new(mnemonic::STA, am))
            .parse(input)
    }
}

/// NOP

impl std::convert::TryFrom<&[u8; 3]> for Instruction<mnemonic::NOP, address_mode::Implied> {
    type Error = String;
    fn try_from(values: &[u8; 3]) -> std::result::Result<Self, Self::Error> {
        match Instruction::new(mnemonic::NOP, address_mode::Implied).parse(values) {
            Ok(parcel::MatchStatus::Match((_, op))) => Ok(op),
            _ => Err(format!("No match found for {}", values[0])),
        }
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::NOP, address_mode::Implied>>
    for Instruction<mnemonic::NOP, address_mode::Implied>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::NOP, address_mode::Implied>> {
        mnemonic::NOP
            .and_then(|_| address_mode::Implied)
            .map(|am| Instruction::new(mnemonic::NOP, am))
            .parse(input)
    }
}

impl Execute<MOS6502> for Instruction<mnemonic::NOP, address_mode::Implied> {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        cpu
    }
}

// JMP

impl std::convert::TryFrom<&[u8; 3]> for Instruction<mnemonic::JMP, address_mode::Absolute> {
    type Error = String;
    fn try_from(values: &[u8; 3]) -> std::result::Result<Self, Self::Error> {
        match Instruction::new(mnemonic::JMP, address_mode::Absolute::default()).parse(values) {
            Ok(parcel::MatchStatus::Match((_, op))) => Ok(op),
            _ => Err(format!("No match found for {}", values[0])),
        }
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::JMP, address_mode::Absolute>>
    for Instruction<mnemonic::JMP, address_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::JMP, address_mode::Absolute>> {
        expect_byte(0x4c)
            .and_then(|_| address_mode::Absolute::default())
            .map(|am| Instruction::new(mnemonic::JMP, am))
            .parse(input)
    }
}
