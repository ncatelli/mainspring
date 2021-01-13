extern crate parcel;
use crate::cpu::{
    mos6502::{register::ProgramCounter, Execute, MOS6502},
    register::Register,
    Cyclable, Offset,
};
use parcel::{parsers::byte::expect_byte, ParseResult, Parser};
use std::fmt::Debug;

pub mod address_mode;
pub mod mnemonic;

#[cfg(test)]
mod tests;

/// Operation functions as a concrete wrapper arround all executable components
/// of a 6502 operation.
pub struct Operation {
    offset: usize,
    cycles: usize,
    callback: Box<dyn Fn(MOS6502) -> MOS6502>,
}

impl Operation {
    pub fn new(offset: usize, cycles: usize, callback: Box<dyn Fn(MOS6502) -> MOS6502>) -> Self {
        Self {
            offset,
            cycles,
            callback,
        }
    }
}

impl Cyclable for Operation {
    fn cycles(&self) -> usize {
        self.cycles
    }
}

impl Offset for Operation {
    fn offset(&self) -> usize {
        self.offset
    }
}

impl Execute<MOS6502> for Operation {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        (self.callback)(cpu)
    }
}

impl std::convert::TryFrom<&[u8; 3]> for Operation {
    type Error = String;
    fn try_from(values: &[u8; 3]) -> std::result::Result<Self, Self::Error> {
        match OperationParser.parse(values) {
            Ok(parcel::MatchStatus::Match((_, op))) => Ok(op),
            _ => Err(format!("No match found for {}", values[0])),
        }
    }
}

/// Provides a wrapper type for parsing byte slices into Operations.
struct OperationParser;

impl<'a> Parser<'a, &'a [u8], Operation> for OperationParser {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Operation> {
        parcel::one_of(vec![
            Instruction::new(mnemonic::NOP, address_mode::Implied).map(|i| {
                Operation::new(i.offset(), i.cycles(), Box::new(move |cpu| i.execute(cpu)))
            }),
            Instruction::new(mnemonic::LDA, address_mode::Immediate::default()).map(|i| {
                Operation::new(i.offset(), i.cycles(), Box::new(move |cpu| i.execute(cpu)))
            }),
            Instruction::new(mnemonic::STA, address_mode::Absolute::default()).map(|i| {
                Operation::new(i.offset(), i.cycles(), Box::new(move |cpu| i.execute(cpu)))
            }),
            Instruction::new(mnemonic::JMP, address_mode::Absolute::default()).map(|i| {
                Operation::new(i.offset(), i.cycles(), Box::new(move |cpu| i.execute(cpu)))
            }),
        ])
        .parse(input)
    }
}

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

impl Execute<MOS6502> for Instruction<mnemonic::LDA, address_mode::Immediate> {
    fn execute(self, _cpu: MOS6502) -> MOS6502 {
        todo!()
    }
}

/// STA

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

impl Execute<MOS6502> for Instruction<mnemonic::STA, address_mode::Absolute> {
    fn execute(self, _cpu: MOS6502) -> MOS6502 {
        todo!()
    }
}

/// NOP

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

impl Execute<MOS6502> for Instruction<mnemonic::JMP, address_mode::Absolute> {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let address_mode::Absolute(addr) = self.address_mode;
        cpu.with_pc_register(ProgramCounter::with_value(addr - self.offset() as u16))
    }
}
