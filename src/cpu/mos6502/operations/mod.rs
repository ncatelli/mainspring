extern crate parcel;
use crate::address_map::Addressable;
use crate::cpu::{
    mos6502::{register::*, Execute, GPRegister, MOS6502},
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

/// Macros to simplify definition of instruction set parsers. by hiding the
/// process of converting an instruction parser to its corresponding operation
macro_rules! inst_to_operation {
    ($inst:expr) => {
        $inst.map(Into::into)
    };
    ($mnemonic:expr, $addrmode:expr) => {
        Instruction::new($mnemonic, $addrmode).map(Into::into)
    };
}

/// Provides a wrapper type for parsing byte slices into Operations.
struct OperationParser;

impl<'a> Parser<'a, &'a [u8], Operation> for OperationParser {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Operation> {
        parcel::one_of(vec![
            inst_to_operation!(mnemonic::NOP, address_mode::Implied),
            inst_to_operation!(mnemonic::LDA, address_mode::Immediate::default()),
            inst_to_operation!(mnemonic::LDA, address_mode::Absolute::default()),
            inst_to_operation!(mnemonic::STA, address_mode::Absolute::default()),
            inst_to_operation!(mnemonic::JMP, address_mode::Absolute::default()),
            inst_to_operation!(mnemonic::JMP, address_mode::Indirect::default()),
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

impl<M, A> Into<Operation> for Instruction<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq + 'static,
    A: Cyclable + Offset + Copy + Debug + PartialEq + 'static,
    Self: Execute<MOS6502>,
{
    fn into(self) -> Operation {
        Operation::new(
            self.offset(),
            self.cycles(),
            Box::new(move |cpu| self.execute(cpu)),
        )
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
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let address_mode::Immediate(value) = self.address_mode;
        cpu.with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(value))
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::LDA, address_mode::Absolute>>
    for Instruction<mnemonic::LDA, address_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::LDA, address_mode::Absolute>> {
        expect_byte(0xad)
            .and_then(|_| address_mode::Absolute::default())
            .map(|am| Instruction::new(mnemonic::LDA, am))
            .parse(input)
    }
}

impl Execute<MOS6502> for Instruction<mnemonic::LDA, address_mode::Absolute> {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let address_mode::Absolute(addr) = self.address_mode;
        let val = cpu.address_map.read(addr);
        cpu.with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(val))
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
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let address_mode::Absolute(addr) = self.address_mode;
        let acc_val = cpu.acc.read();

        let mut cpu = cpu;
        cpu.address_map.write(addr, acc_val).unwrap();
        cpu
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

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::JMP, address_mode::Indirect>>
    for Instruction<mnemonic::JMP, address_mode::Indirect>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::JMP, address_mode::Indirect>> {
        expect_byte(0x6c)
            .and_then(|_| address_mode::Indirect::default())
            .map(|am| Instruction::new(mnemonic::JMP, am))
            .parse(input)
    }
}

impl Execute<MOS6502> for Instruction<mnemonic::JMP, address_mode::Indirect> {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let address_mode::Indirect(indirect_addr) = self.address_mode;
        let lsb = cpu.address_map.read(indirect_addr);
        let msb = cpu.address_map.read(indirect_addr + 1);
        let addr = u16::from_le_bytes([lsb, msb]);
        cpu.with_pc_register(ProgramCounter::with_value(addr - self.offset() as u16))
    }
}
