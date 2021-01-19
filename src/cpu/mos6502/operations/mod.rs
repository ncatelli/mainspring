extern crate parcel;
use crate::address_map::Addressable;
use crate::cpu::{
    mos6502::{microcode::*, register::*, Generate, MOS6502},
    register::Register,
    Cyclable, Offset,
};
use parcel::{parsers::byte::expect_byte, ParseResult, Parser};
use std::fmt::Debug;
use std::num::Wrapping;
use std::ops::{Add, Sub};

pub mod address_mode;
pub mod mnemonic;

#[cfg(test)]
mod tests;

/// Represents a response that will yield a result that might or might not
/// result in wrapping, overflow or negative values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Operand<T> {
    carry: bool,
    negative: bool,
    zero: bool,
    inner: T,
}

impl<T> Operand<T> {
    fn with_flags(inner: T, carry: bool, negative: bool, zero: bool) -> Self {
        Self {
            carry,
            negative,
            zero,
            inner,
        }
    }

    fn unwrap(self) -> T {
        self.inner
    }
}

impl<T> PartialEq<T> for Operand<T>
where
    T: PartialEq + Copy,
{
    fn eq(&self, rhs: &T) -> bool {
        let lhs = self.unwrap();
        lhs == *rhs
    }
}

impl Operand<u8> {
    fn new(inner: u8) -> Self {
        Self {
            carry: false,
            negative: inner > 127,
            zero: inner == 0,
            inner,
        }
    }
}

impl Sub for Operand<u8> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let difference = (Wrapping(lhs) - Wrapping(rhs)).0;
        let carry = (lhs as u16 + rhs as u16) > 255;
        let negative = difference > 127; // most significant bit set
        let zero = lhs == rhs;

        Self::with_flags(difference, carry, negative, zero)
    }
}

impl Add for Operand<u8> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let sum = (Wrapping(lhs) + Wrapping(rhs)).0;
        let carry = (lhs as u16 + rhs as u16) > 255;
        let negative = sum > 127; // most significant bit set
        let zero = lhs == rhs;

        Self::with_flags(sum, carry, negative, zero)
    }
}

/// MOps functions as a concrete wrapper around a microcode operation with
/// metadata around sizing and cycles. This trait does NOT represent a cycle
/// but rather the microcode equivalent of a CPU instruction.
#[derive(Debug, Clone, PartialEq)]
pub struct MOps {
    offset: usize,
    cycles: usize,
    microcode: Vec<Microcode>,
}

impl MOps {
    pub fn new(offset: usize, cycles: usize, microcode: Vec<Microcode>) -> Self {
        Self {
            offset,
            cycles,
            microcode,
        }
    }
}

impl Cyclable for MOps {
    fn cycles(&self) -> usize {
        self.cycles
    }
}

impl Offset for MOps {
    fn offset(&self) -> usize {
        self.offset
    }
}

impl From<MOps> for Vec<Vec<Microcode>> {
    fn from(src: MOps) -> Self {
        let cycles = src.cycles();
        let offset = src.offset() as u16;
        let mut mcs = vec![vec![]; cycles - 1];
        let mut microcode = src.microcode;
        microcode.push(Microcode::Inc16bitRegister(Inc16bitRegister::new(
            WordRegisters::PC,
            offset,
        )));

        mcs.push(microcode);
        mcs
    }
}

/// Operation functions as a concrete wrapper around all executable components
/// of a 6502 operation.
pub struct Operation {
    offset: usize,
    cycles: usize,
    generator: Box<dyn Fn(&MOS6502) -> MOps>,
}

impl Operation {
    pub fn new(offset: usize, cycles: usize, generator: Box<dyn Fn(&MOS6502) -> MOps>) -> Self {
        Self {
            offset,
            cycles,
            generator,
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

impl Generate<MOS6502, MOps> for Operation {
    fn generate(self, cpu: &MOS6502) -> MOps {
        (self.generator)(cpu)
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
            inst_to_operation!(mnemonic::CMP, address_mode::Immediate::default()),
            inst_to_operation!(mnemonic::JMP, address_mode::Absolute::default()),
            inst_to_operation!(mnemonic::JMP, address_mode::Indirect::default()),
            inst_to_operation!(mnemonic::LDA, address_mode::Immediate::default()),
            inst_to_operation!(mnemonic::LDA, address_mode::ZeroPage::default()),
            inst_to_operation!(mnemonic::LDA, address_mode::ZeroPageIndexedWithX::default()),
            inst_to_operation!(mnemonic::LDA, address_mode::Absolute::default()),
            inst_to_operation!(mnemonic::NOP, address_mode::Implied),
            inst_to_operation!(mnemonic::STA, address_mode::Absolute::default()),
            inst_to_operation!(mnemonic::TXA, address_mode::Implied),
        ])
        .parse(input)
    }
}

/// Instruction takes a mnemonic and address mode as arguments for sizing
/// and operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction<M, A>
where
    M: Offset + Copy + Debug + PartialEq,
    A: Offset + Copy + Debug + PartialEq,
{
    mnemonic: M,
    address_mode: A,
}

impl<M, A> Instruction<M, A>
where
    M: Offset + Copy + Debug + PartialEq,
    A: Offset + Copy + Debug + PartialEq,
{
    pub fn new(mnemonic: M, address_mode: A) -> Self {
        Instruction {
            mnemonic,
            address_mode,
        }
    }
}

impl<M, A> Offset for Instruction<M, A>
where
    M: Offset + Copy + Debug + PartialEq,
    A: Offset + Copy + Debug + PartialEq,
{
    fn offset(&self) -> usize {
        self.mnemonic.offset() + self.address_mode.offset()
    }
}

impl<M, A> Into<Operation> for Instruction<M, A>
where
    M: Offset + Copy + Debug + PartialEq + 'static,
    A: Offset + Copy + Debug + PartialEq + 'static,
    Self: Generate<MOS6502, MOps> + Cyclable + 'static,
{
    fn into(self) -> Operation {
        Operation::new(
            self.offset(),
            self.cycles(),
            Box::new(move |cpu| self.generate(cpu)),
        )
    }
}

// CMP

impl Cyclable for Instruction<mnemonic::CMP, address_mode::Immediate> {
    fn cycles(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::CMP, address_mode::Immediate>>
    for Instruction<mnemonic::CMP, address_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::CMP, address_mode::Immediate>> {
        expect_byte(0xc9)
            .and_then(|_| address_mode::Immediate::default())
            .map(|am| Instruction::new(mnemonic::CMP, am))
            .parse(input)
    }
}

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, address_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let address_mode::Immediate(am_value) = self.address_mode;
        let rhs = Operand::new(am_value);
        let lhs = Operand::new(cpu.acc.read());
        let carry = lhs >= rhs;
        let diff = lhs - rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, diff.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, diff.zero),
            ],
        )
    }
}

/// LDA

impl Cyclable for Instruction<mnemonic::LDA, address_mode::Immediate> {
    fn cycles(&self) -> usize {
        2
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

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, address_mode::Immediate> {
    fn generate(self, _: &MOS6502) -> MOps {
        let address_mode::Immediate(am_val) = self.address_mode;
        let value = Operand::new(am_val);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                Microcode::Write8bitRegister(Write8bitRegister::new(
                    ByteRegisters::ACC,
                    value.unwrap(),
                )),
            ],
        )
    }
}

impl Cyclable for Instruction<mnemonic::LDA, address_mode::ZeroPage> {
    fn cycles(&self) -> usize {
        3
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::LDA, address_mode::ZeroPage>>
    for Instruction<mnemonic::LDA, address_mode::ZeroPage>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::LDA, address_mode::ZeroPage>> {
        expect_byte(0xa5)
            .and_then(|_| address_mode::ZeroPage::default())
            .map(|am| Instruction::new(mnemonic::LDA, am))
            .parse(input)
    }
}

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, address_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let address_mode::ZeroPage(addr) = self.address_mode;
        let value = Operand::new(cpu.address_map.read(addr as u16));

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                Microcode::Write8bitRegister(Write8bitRegister::new(
                    ByteRegisters::ACC,
                    value.unwrap(),
                )),
            ],
        )
    }
}

impl Cyclable for Instruction<mnemonic::LDA, address_mode::ZeroPageIndexedWithX> {
    fn cycles(&self) -> usize {
        4
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::LDA, address_mode::ZeroPageIndexedWithX>>
    for Instruction<mnemonic::LDA, address_mode::ZeroPageIndexedWithX>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::LDA, address_mode::ZeroPageIndexedWithX>> {
        expect_byte(0xb5)
            .and_then(|_| address_mode::ZeroPageIndexedWithX::default())
            .map(|am| Instruction::new(mnemonic::LDA, am))
            .parse(input)
    }
}

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, address_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let address_mode::ZeroPageIndexedWithX(addr) = self.address_mode;
        let x = cpu.x.read();
        let indirect_value = cpu.address_map.read((addr + x) as u16);
        let value = Operand::new(indirect_value);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                Microcode::Write8bitRegister(Write8bitRegister::new(
                    ByteRegisters::ACC,
                    value.unwrap(),
                )),
            ],
        )
    }
}

impl Cyclable for Instruction<mnemonic::LDA, address_mode::Absolute> {
    fn cycles(&self) -> usize {
        4
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

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, address_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let address_mode::Absolute(addr) = self.address_mode;
        let value = Operand::new(cpu.address_map.read(addr));
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                Microcode::Write8bitRegister(Write8bitRegister::new(
                    ByteRegisters::ACC,
                    value.unwrap(),
                )),
            ],
        )
    }
}

/// STA

impl Cyclable for Instruction<mnemonic::STA, address_mode::Absolute> {
    fn cycles(&self) -> usize {
        4
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

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, address_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let address_mode::Absolute(addr) = self.address_mode;
        let acc_val = cpu.acc.read();
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![Microcode::WriteMemory(WriteMemory::new(addr, acc_val))],
        )
    }
}

// NOP

impl Cyclable for Instruction<mnemonic::NOP, address_mode::Implied> {
    fn cycles(&self) -> usize {
        2
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

impl Generate<MOS6502, MOps> for Instruction<mnemonic::NOP, address_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(self.offset(), self.cycles(), vec![])
    }
}

// JMP

impl Cyclable for Instruction<mnemonic::JMP, address_mode::Absolute> {
    fn cycles(&self) -> usize {
        3
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

impl Generate<MOS6502, MOps> for Instruction<mnemonic::JMP, address_mode::Absolute> {
    fn generate(self, _: &MOS6502) -> MOps {
        let address_mode::Absolute(addr) = self.address_mode;
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![Microcode::Write16bitRegister(Write16bitRegister::new(
                WordRegisters::PC,
                addr - self.offset() as u16,
            ))],
        )
    }
}

impl Cyclable for Instruction<mnemonic::JMP, address_mode::Indirect> {
    fn cycles(&self) -> usize {
        5
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

impl Generate<MOS6502, MOps> for Instruction<mnemonic::JMP, address_mode::Indirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let address_mode::Indirect(indirect_addr) = self.address_mode;
        let lsb = cpu.address_map.read(indirect_addr);
        let msb = cpu.address_map.read(indirect_addr + 1);
        let addr = u16::from_le_bytes([lsb, msb]);
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![Microcode::Write16bitRegister(Write16bitRegister::new(
                WordRegisters::PC,
                addr - self.offset() as u16,
            ))],
        )
    }
}

impl Cyclable for Instruction<mnemonic::TXA, address_mode::Implied> {
    fn cycles(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], Instruction<mnemonic::TXA, address_mode::Implied>>
    for Instruction<mnemonic::TXA, address_mode::Implied>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Instruction<mnemonic::TXA, address_mode::Implied>> {
        expect_byte(0x8a)
            .and_then(|_| address_mode::Implied)
            .map(|am| Instruction::new(mnemonic::TXA, am))
            .parse(input)
    }
}

impl Generate<MOS6502, MOps> for Instruction<mnemonic::TXA, address_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.acc.read());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                Microcode::Write8bitRegister(Write8bitRegister::new(
                    ByteRegisters::X,
                    value.unwrap(),
                )),
            ],
        )
    }
}
