extern crate parcel;
use crate::address_map::{page::Page, Addressable};
use crate::cpu::{
    mos6502::{microcode::Microcode, register::*, Generate, IRQ_VECTOR_HH, IRQ_VECTOR_LL, MOS6502},
    register::Register,
    Cyclable, Offset,
};
use parcel::{parsers::byte::expect_byte, ParseResult, Parser};
use std::fmt::Debug;
use std::num::Wrapping;

pub mod addressing_mode;
pub mod mnemonic;

#[cfg(test)]
mod tests;

/// Takes two numerical values returning whether the bit is set for a specific
/// place.
macro_rules! bit_is_set {
    ($value:expr, $place:expr) => {
        (($value >> $place) & 1) == 1
    };
}

/// This Trait provides addition that that signifies the overflow of a twos complement number.
trait AddTwosComplement<Rhs = Self> {
    type Output;

    /// Adds the left and right hand sides, returning the value and the boolean
    /// representation of the formula
    /// (!LHSMSB & !RHSMSB & C) || (LHSMSB & RHSMSB & !C).
    fn twos_complement_add(self, rhs: Rhs, carry: bool) -> (Self::Output, bool);
}

/// This Trait provides twos-complement subtraction.
trait SubTwosComplement<Rhs = Self> {
    type Output;

    /// subtracts the left and right hand sides returning a cary using twos complement
    fn twos_complement_sub(self, rhs: Rhs, carry: bool) -> (Self::Output, bool);
}

/// This Trait provides a rotate-left operation using the carry bit as shift
/// in and the 7 bit as the shift out.
trait Rol<Rhs = Self> {
    type Output;

    /// Shifts all bits left, rotating in the carry bit from the right and the
    /// 7th bit of the lhs to the carry bit.
    fn rol(self, rhs: Rhs, carry: bool) -> Self::Output;
}

/// This Trait provides a rotate-right operation using the carry bit as shift
/// in and the 0 bit as the shift out.
trait Ror<Rhs = Self> {
    type Output;

    /// Shifts all bits right, rotating in the carry bit from the left and the
    /// 0th bit of the lhs to the carry bit.
    fn ror(self, rhs: Rhs, carry: bool) -> Self::Output;
}

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
    pub fn new(inner: u8) -> Self {
        Self {
            carry: false,
            negative: ((inner >> 7) & 1) == 1, // most significant bit set
            zero: inner == 0,
            inner,
        }
    }
}

impl std::ops::Add for Operand<u8> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let (sum, carry) = lhs.overflowing_add(rhs);
        let negative = ((sum >> 7) & 1) == 1; // most significant bit set
        let zero = sum == 0;

        Self::with_flags(sum, carry, negative, zero)
    }
}

impl AddTwosComplement for Operand<u8> {
    type Output = Self;

    #[allow(clippy::nonminimal_bool)]
    fn twos_complement_add(self, other: Self, carry: bool) -> (Self::Output, bool) {
        let sum = self + other;
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let overflow = (!bit_is_set!(lhs, 7) && !bit_is_set!(rhs, 7) && carry)
            || (bit_is_set!(lhs, 7) && bit_is_set!(rhs, 7) && !carry);

        (sum, overflow)
    }
}

impl std::ops::Sub for Operand<u8> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let (difference, carry) = lhs.overflowing_sub(rhs);
        let negative = ((difference >> 7) & 1) == 1; // most significant bit set
        let zero = difference == 0;

        Self::with_flags(difference, carry, negative, zero)
    }
}

impl SubTwosComplement for Operand<u8> {
    type Output = Self;

    fn twos_complement_sub(self, other: Self, carry: bool) -> (Self::Output, bool) {
        let carry_bit = carry as u8; // 1 if true 0 if false
        let rhs_ones_complement = Operand::new(255 - other.unwrap()) + Operand::new(carry_bit);
        self.twos_complement_add(rhs_ones_complement, carry)
    }
}

impl std::ops::BitAnd for Operand<u8> {
    type Output = Self;

    fn bitand(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let value = lhs & rhs;
        Self::new(value)
    }
}

impl std::ops::BitOr for Operand<u8> {
    type Output = Self;

    fn bitor(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let value = lhs | rhs;
        Self::new(value)
    }
}

impl std::ops::BitXor for Operand<u8> {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let value = lhs ^ rhs;
        Self::new(value)
    }
}

impl Rol for Operand<u8> {
    type Output = Self;

    fn rol(self, other: Self, carry: bool) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let carry_in = carry as u8; // bool translates to 1 or 0 emulating 0th bit.
        let carry_out = bit_is_set!(lhs, 7);
        let shifted = Operand::new(lhs << rhs | carry_in);

        Operand::with_flags(shifted.unwrap(), carry_out, shifted.negative, shifted.zero)
    }
}

impl Ror for Operand<u8> {
    type Output = Self;

    fn ror(self, other: Self, carry: bool) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let carry_in = (carry as u8) << 7;
        let carry_out = bit_is_set!(lhs, 0);
        let shifted = Operand::new(lhs >> rhs | carry_in);

        Operand::with_flags(shifted.unwrap(), carry_out, shifted.negative, shifted.zero)
    }
}

impl std::ops::Shl for Operand<u8> {
    type Output = Self;

    fn shl(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let carry = bit_is_set!(lhs, 7); // if the msb is set, shift to carry
        let value = Operand::new(lhs << rhs);

        Operand::with_flags(value.unwrap(), carry, value.negative, value.zero)
    }
}

impl std::ops::Shr for Operand<u8> {
    type Output = Self;

    fn shr(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.unwrap(), other.unwrap());
        let carry = bit_is_set!(lhs, 0); // if the lsb is set, shift to carry
        let value = Operand::new(lhs >> rhs);

        Operand::with_flags(value.unwrap(), carry, value.negative, value.zero)
    }
}

// addressing mode Unpackers

/// Provides a wrapper around the operation of unpacking an addressing mode and
/// adding an indirect offset to it. This appropriately handles for overflow
/// and returns the address as a u16.
fn add_index_to_address(addr: u16, index: u8) -> u16 {
    addr.overflowing_add(index as u16).0
}

/// Provides a wrapper around the operation of unpacking a zeropage addressing
/// mode and adding an indirect offset to it. This appropriately handles for
/// overflow and returns the address as a u16.
fn add_index_to_zeropage_address(zeropage_addr: u8, index: u8) -> u16 {
    zeropage_addr.overflowing_add(index).0 as u16
}

/// Provides a wrapper around the common operation of dereferencing an indexed
/// indirect address. This is effectively taking the value at
/// (Operand + Index, addr at Operand + Index + 1).
fn dereference_indexed_indirect_address(cpu: &MOS6502, base_addr: u8, index: u8) -> u16 {
    u16::from_le_bytes([
        cpu.address_map
            .read(base_addr.overflowing_add(index).0 as u16),
        cpu.address_map
            .read(base_addr.overflowing_add(index + 1).0 as u16),
    ])
}

/// Provides a wrapper around the operation of dereferencing an indirect
/// address and then adding an index to that indirect address. This is
/// effectively the value at (Operand, Operand + 1) + Index.
fn dereference_indirect_indexed_address(cpu: &MOS6502, base_addr: u8, index: u8) -> u16 {
    u16::from_le_bytes([
        cpu.address_map.read(base_addr as u16),
        cpu.address_map.read(base_addr.overflowing_add(1).0 as u16),
    ]) + index as u16
}

/// Provides a wrapper around the common operation of dereferencing and address
/// mode and retrieving the value stored at the specified address from the
/// address map. This value is then returned in a wrapper Operand.
fn dereference_address_to_operand(cpu: &MOS6502, addr: u16, index: u8) -> Operand<u8> {
    Operand::new(
        cpu.address_map
            .read(add_index_to_address(addr as u16, index)),
    )
}

/// Provides a wrapper around generating a 16-bit address from the stack
/// pointer. This exists as a function solely to not lose intent in the type
/// conversion to u16.
fn stack_pointer_from_byte_value(value: u8) -> u16 {
    u16::from_le_bytes([value, 0x01])
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
        let mut mcs = vec![Vec::<Microcode>::new(); cycles - 1];

        mcs.push(
            src.microcode
                .into_iter()
                .chain(
                    vec![gen_inc_16bit_register_microcode!(WordRegisters::PC, offset)].into_iter(),
                )
                .collect(),
        );
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
            inst_to_operation!(mnemonic::ADC, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::ADC,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::ADC,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::ADC, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::ADC, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::ADC, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::ADC, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::ADC,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::AND, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::AND,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::AND,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::AND, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::AND, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::AND, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::AND, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::AND,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::ASL, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::ASL,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::ASL, addressing_mode::Accumulator),
            inst_to_operation!(mnemonic::ASL, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::ASL,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::BCC, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::BCS, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::BEQ, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::BMI, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::BIT, addressing_mode::Absolute::default()),
            inst_to_operation!(mnemonic::BIT, addressing_mode::ZeroPage::default()),
            inst_to_operation!(mnemonic::BNE, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::BPL, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::BRK, addressing_mode::Implied::default()),
            inst_to_operation!(mnemonic::BVC, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::BVS, addressing_mode::Relative::default()),
            inst_to_operation!(mnemonic::CLC, addressing_mode::Implied),
            inst_to_operation!(mnemonic::CLD, addressing_mode::Implied),
            inst_to_operation!(mnemonic::CLI, addressing_mode::Implied),
            inst_to_operation!(mnemonic::CLV, addressing_mode::Implied),
            inst_to_operation!(mnemonic::CMP, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::CMP,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::CMP,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::CMP, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::CMP, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::CMP, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::CMP, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::CMP,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::CPX, addressing_mode::Absolute::default()),
            inst_to_operation!(mnemonic::CPX, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::CPX, addressing_mode::ZeroPage::default()),
            inst_to_operation!(mnemonic::CPY, addressing_mode::Absolute::default()),
            inst_to_operation!(mnemonic::CPY, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::CPY, addressing_mode::ZeroPage::default()),
            inst_to_operation!(mnemonic::DEC, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::DEC,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::DEC, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::DEC,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::DEX, addressing_mode::Implied),
            inst_to_operation!(mnemonic::DEY, addressing_mode::Implied),
            inst_to_operation!(mnemonic::EOR, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::EOR,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::EOR,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::EOR, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::EOR, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::EOR, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::EOR, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::EOR,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::INC, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::INC,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::INC, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::INC,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::INX, addressing_mode::Implied),
            inst_to_operation!(mnemonic::INY, addressing_mode::Implied),
            inst_to_operation!(mnemonic::JMP, addressing_mode::Absolute::default()),
            inst_to_operation!(mnemonic::JMP, addressing_mode::Indirect::default()),
            inst_to_operation!(mnemonic::JSR, addressing_mode::Absolute::default()),
            inst_to_operation!(mnemonic::LDA, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::LDA,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::LDA,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::LDA, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::LDA, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::LDA, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::LDA, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::LDA,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::LDX, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::LDX,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::LDX, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::LDX, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::LDX,
                addressing_mode::ZeroPageIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::LDY, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::LDY,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::LDY, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::LDY, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::LDY,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::LSR, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::LSR,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::LSR, addressing_mode::Accumulator),
            inst_to_operation!(mnemonic::LSR, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::LSR,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::NOP, addressing_mode::Implied),
            inst_to_operation!(mnemonic::ORA, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::ORA,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::ORA,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::ORA, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::ORA, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::ORA, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::ORA, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::ORA,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::PHA, addressing_mode::Implied),
            inst_to_operation!(mnemonic::PHP, addressing_mode::Implied),
            inst_to_operation!(mnemonic::PLA, addressing_mode::Implied),
            inst_to_operation!(mnemonic::PLP, addressing_mode::Implied),
            inst_to_operation!(mnemonic::ROL, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::ROL,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::ROL, addressing_mode::Accumulator),
            inst_to_operation!(mnemonic::ROL, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::ROL,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::ROR, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::ROR,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::ROR, addressing_mode::Accumulator),
            inst_to_operation!(mnemonic::ROR, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::ROR,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::RTI, addressing_mode::Implied),
            inst_to_operation!(mnemonic::RTS, addressing_mode::Implied),
            inst_to_operation!(mnemonic::SBC, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::SBC,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::SBC,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::SBC, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::SBC, addressing_mode::Immediate::default()),
            inst_to_operation!(mnemonic::SBC, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::SBC, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::SBC,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::STA, addressing_mode::Absolute::default()),
            inst_to_operation!(
                mnemonic::STA,
                addressing_mode::AbsoluteIndexedWithX::default()
            ),
            inst_to_operation!(
                mnemonic::STA,
                addressing_mode::AbsoluteIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::STA, addressing_mode::IndirectYIndexed::default()),
            inst_to_operation!(mnemonic::STA, addressing_mode::XIndexedIndirect::default()),
            inst_to_operation!(mnemonic::STA, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::STA,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::STX, addressing_mode::Absolute::default()),
            inst_to_operation!(mnemonic::STX, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::STX,
                addressing_mode::ZeroPageIndexedWithY::default()
            ),
            inst_to_operation!(mnemonic::STY, addressing_mode::Absolute::default()),
            inst_to_operation!(mnemonic::STY, addressing_mode::ZeroPage::default()),
            inst_to_operation!(
                mnemonic::STY,
                addressing_mode::ZeroPageIndexedWithX::default()
            ),
            inst_to_operation!(mnemonic::SEC, addressing_mode::Implied),
            inst_to_operation!(mnemonic::SED, addressing_mode::Implied),
            inst_to_operation!(mnemonic::SEI, addressing_mode::Implied),
            inst_to_operation!(mnemonic::TAX, addressing_mode::Implied),
            inst_to_operation!(mnemonic::TAY, addressing_mode::Implied),
            inst_to_operation!(mnemonic::TSX, addressing_mode::Implied),
            inst_to_operation!(mnemonic::TXA, addressing_mode::Implied),
            inst_to_operation!(mnemonic::TXS, addressing_mode::Implied),
            inst_to_operation!(mnemonic::TYA, addressing_mode::Implied),
        ])
        .parse(input)
    }
}

/// Instruction takes a mnemonic and addressing mode as arguments for sizing
/// and operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction<M, A>
where
    M: Offset + Copy + Debug + PartialEq,
    A: Offset + Copy + Debug + PartialEq,
{
    mnemonic: M,
    addressing_mode: A,
}

impl<M, A> Instruction<M, A>
where
    M: Offset + Copy + Debug + PartialEq,
    A: Offset + Copy + Debug + PartialEq,
{
    pub fn new(mnemonic: M, addressing_mode: A) -> Self {
        Instruction {
            mnemonic,
            addressing_mode,
        }
    }
}

impl<M, A> Offset for Instruction<M, A>
where
    M: Offset + Copy + Debug + PartialEq,
    A: Offset + Copy + Debug + PartialEq,
{
    fn offset(&self) -> usize {
        self.mnemonic.offset() + self.addressing_mode.offset()
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

macro_rules! gen_instruction_cycles_and_parser {
    ($mnemonic:ty, $addressing_mode:ty, $opcode:literal, $cycles:literal) => {
        impl Cyclable for Instruction<$mnemonic, $addressing_mode> {
            fn cycles(&self) -> usize {
                $cycles
            }
        }

        impl<'a> Parser<'a, &'a [u8], Instruction<$mnemonic, $addressing_mode>>
            for Instruction<$mnemonic, $addressing_mode>
        {
            fn parse(
                &self,
                input: &'a [u8],
            ) -> ParseResult<&'a [u8], Instruction<$mnemonic, $addressing_mode>> {
                // If the expected opcode and addressing mode match, map it to a
                // corresponding Instruction.
                parcel::map(
                    parcel::and_then(expect_byte($opcode), |_| <$addressing_mode>::default()),
                    |am| Instruction::new(<$mnemonic>::default(), am),
                )
                .parse(input)
            }
        }
    };
}

// Arithmetic Operations

// ADC

gen_instruction_cycles_and_parser!(mnemonic::ADC, addressing_mode::Absolute, 0x6d, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ADC,
    addressing_mode::AbsoluteIndexedWithX,
    0x7d,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ADC,
    addressing_mode::AbsoluteIndexedWithY,
    0x79,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, cpu.y.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ADC, addressing_mode::IndirectYIndexed, 0x71, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let zpage_base_addr = self.addressing_mode.unwrap();
        let indirect_addr =
            dereference_indirect_indexed_address(cpu, zpage_base_addr, cpu.y.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(zpage_base_addr as u16).contains(indirect_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ADC, addressing_mode::Immediate, 0x69, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(self.addressing_mode.unwrap());

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ADC, addressing_mode::XIndexedIndirect, 0x61, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ADC, addressing_mode::ZeroPage, 0x65, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), 0);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ADC,
    addressing_mode::ZeroPageIndexedWithX,
    0x75,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ADC, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_zeropage_address(addr, cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_add(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// SBC

gen_instruction_cycles_and_parser!(mnemonic::SBC, addressing_mode::Absolute, 0xed, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::SBC,
    addressing_mode::AbsoluteIndexedWithX,
    0xFD,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::SBC,
    addressing_mode::AbsoluteIndexedWithY,
    0xF9,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, cpu.y.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::SBC, addressing_mode::IndirectYIndexed, 0xf1, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let zpage_base_addr = self.addressing_mode.unwrap();
        let indirect_addr =
            dereference_indirect_indexed_address(cpu, zpage_base_addr, cpu.y.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(zpage_base_addr as u16).contains(indirect_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::SBC, addressing_mode::Immediate, 0xe9, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(self.addressing_mode.unwrap());

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::SBC, addressing_mode::XIndexedIndirect, 0xe1, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::SBC, addressing_mode::ZeroPage, 0xe5, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), 0);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::SBC,
    addressing_mode::ZeroPageIndexedWithX,
    0xf5,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SBC, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_zeropage_address(addr, cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);

        // calculate overflow
        let (value, overflow) = lhs.twos_complement_sub(rhs, cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// Bit-wise Operations

// AND

gen_instruction_cycles_and_parser!(mnemonic::AND, addressing_mode::Absolute, 0x2d, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);
        let value = lhs & rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::AND,
    addressing_mode::AbsoluteIndexedWithX,
    0x3d,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, index);
        let value = lhs & rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::AND,
    addressing_mode::AbsoluteIndexedWithY,
    0x39,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, index);
        let value = lhs & rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::AND, addressing_mode::IndirectYIndexed, 0x31, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let zpage_base_addr = self.addressing_mode.unwrap();
        let indirect_addr =
            dereference_indirect_indexed_address(cpu, zpage_base_addr, cpu.y.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));
        let value = lhs & rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(zpage_base_addr as u16).contains(indirect_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::AND, addressing_mode::Immediate, 0x29, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(self.addressing_mode.unwrap());
        let value = lhs & rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::AND, addressing_mode::XIndexedIndirect, 0x21, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));
        let value = lhs & rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::AND, addressing_mode::ZeroPage, 0x25, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);
        let value = lhs & rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::AND,
    addressing_mode::ZeroPageIndexedWithX,
    0x35,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::AND, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);
        let value = lhs & rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// ASL

gen_instruction_cycles_and_parser!(mnemonic::ASL, addressing_mode::Absolute, 0x0e, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ASL, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = dereference_address_to_operand(cpu, addr, 0) << Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ASL,
    addressing_mode::AbsoluteIndexedWithX,
    0x1e,
    7
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ASL, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, addr, index) << Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ASL, addressing_mode::Accumulator, 0x0a, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ASL, addressing_mode::Accumulator> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.acc.read()) << Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ASL, addressing_mode::ZeroPage, 0x06, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ASL, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let value = dereference_address_to_operand(cpu, addr, 0) << Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ASL,
    addressing_mode::ZeroPageIndexedWithX,
    0x16,
    6
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ASL, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0) << Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

// BIT

gen_instruction_cycles_and_parser!(mnemonic::BIT, addressing_mode::Absolute, 0x2c, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BIT, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);
        let negative = bit_is_set!(rhs.unwrap(), 7);
        let overflow = bit_is_set!(rhs.unwrap(), 6);
        let value = lhs & rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::BIT, addressing_mode::ZeroPage, 0x24, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BIT, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), 0);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, 0);
        let negative = bit_is_set!(rhs.unwrap(), 7);
        let overflow = bit_is_set!(rhs.unwrap(), 6);
        let value = lhs & rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, overflow),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
            ],
        )
    }
}

// EOR

gen_instruction_cycles_and_parser!(mnemonic::EOR, addressing_mode::Absolute, 0x4d, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);
        let value = lhs ^ rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::EOR,
    addressing_mode::AbsoluteIndexedWithX,
    0x5d,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, index);
        let value = lhs ^ rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::EOR,
    addressing_mode::AbsoluteIndexedWithY,
    0x59,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, index);
        let value = lhs ^ rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::EOR, addressing_mode::IndirectYIndexed, 0x51, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let zpage_base_addr = self.addressing_mode.unwrap();
        let indirect_addr =
            dereference_indirect_indexed_address(cpu, zpage_base_addr, cpu.y.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));
        let value = lhs ^ rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(zpage_base_addr as u16).contains(indirect_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::EOR, addressing_mode::Immediate, 0x49, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(self.addressing_mode.unwrap());
        let value = lhs ^ rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::EOR, addressing_mode::XIndexedIndirect, 0x41, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));
        let value = lhs ^ rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::EOR, addressing_mode::ZeroPage, 0x45, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);
        let value = lhs ^ rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::EOR,
    addressing_mode::ZeroPageIndexedWithX,
    0x55,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::EOR, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);
        let value = lhs ^ rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// LSR

gen_instruction_cycles_and_parser!(mnemonic::LSR, addressing_mode::Absolute, 0x4e, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LSR, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = dereference_address_to_operand(cpu, addr, 0) >> Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LSR,
    addressing_mode::AbsoluteIndexedWithX,
    0x5e,
    7
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LSR, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, addr, index) >> Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LSR, addressing_mode::Accumulator, 0x4a, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LSR, addressing_mode::Accumulator> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.acc.read()) >> Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LSR, addressing_mode::ZeroPage, 0x46, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LSR, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let value = dereference_address_to_operand(cpu, addr, 0) >> Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LSR,
    addressing_mode::ZeroPageIndexedWithX,
    0x56,
    6
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LSR, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0) >> Operand::new(1u8);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

// ORA

gen_instruction_cycles_and_parser!(mnemonic::ORA, addressing_mode::Absolute, 0x0d, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);
        let value = lhs | rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ORA,
    addressing_mode::AbsoluteIndexedWithX,
    0x1d,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, index);
        let value = lhs | rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ORA,
    addressing_mode::AbsoluteIndexedWithY,
    0x19,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, addr, index);
        let value = lhs | rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ORA, addressing_mode::IndirectYIndexed, 0x11, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let zpage_base_addr = self.addressing_mode.unwrap();
        let indirect_addr =
            dereference_indirect_indexed_address(cpu, zpage_base_addr, cpu.y.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));
        let value = lhs | rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(zpage_base_addr as u16).contains(indirect_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ORA, addressing_mode::Immediate, 0x09, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(self.addressing_mode.unwrap());
        let value = lhs | rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ORA, addressing_mode::XIndexedIndirect, 0x01, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), cpu.x.read());
        let lhs = Operand::new(cpu.acc.read());
        let rhs = Operand::new(cpu.address_map.read(indirect_addr));
        let value = lhs | rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ORA, addressing_mode::ZeroPage, 0x05, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);
        let value = lhs | rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ORA,
    addressing_mode::ZeroPageIndexedWithX,
    0x15,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ORA, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let lhs = Operand::new(cpu.acc.read());
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);
        let value = lhs | rhs;

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// ROL

gen_instruction_cycles_and_parser!(mnemonic::ROL, addressing_mode::Absolute, 0x2e, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROL, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let lhs = dereference_address_to_operand(cpu, addr, 0);
        let value = lhs.rol(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ROL,
    addressing_mode::AbsoluteIndexedWithX,
    0x3e,
    7
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROL, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let lhs = dereference_address_to_operand(cpu, addr, index);
        let value = lhs.rol(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ROL, addressing_mode::Accumulator, 0x2a, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROL, addressing_mode::Accumulator> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.acc.read()).rol(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ROL, addressing_mode::ZeroPage, 0x26, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROL, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let lhs = dereference_address_to_operand(cpu, addr, 0);
        let value = lhs.rol(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ROL,
    addressing_mode::ZeroPageIndexedWithX,
    0x36,
    6
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROL, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let lhs = dereference_address_to_operand(cpu, indexed_addr, 0);
        let value = lhs.rol(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

// ROR

gen_instruction_cycles_and_parser!(mnemonic::ROR, addressing_mode::Absolute, 0x6e, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROR, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value =
            dereference_address_to_operand(cpu, addr, 0).ror(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ROR,
    addressing_mode::AbsoluteIndexedWithX,
    0x7e,
    7
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROR, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value =
            dereference_address_to_operand(cpu, addr, index).ror(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ROR, addressing_mode::Accumulator, 0x6a, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROR, addressing_mode::Accumulator> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.acc.read()).ror(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::ROR, addressing_mode::ZeroPage, 0x66, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROR, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let value =
            dereference_address_to_operand(cpu, addr, 0).ror(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::ROR,
    addressing_mode::ZeroPageIndexedWithX,
    0x76,
    6
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::ROR, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0)
            .ror(Operand::new(1u8), cpu.ps.carry);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, value.carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

// Branching

fn branch_on_case(
    cond: bool,
    branch_offset: i8,
    inst_offset: usize,
    cycles: usize,
    cpu: &MOS6502,
) -> MOps {
    let jmp_on_eq = (Wrapping(cpu.pc.read()) + Wrapping(branch_offset as u16)).0;
    let mc = if cond {
        vec![gen_write_16bit_register_microcode!(
            WordRegisters::PC,
            // handle for underflow
            jmp_on_eq
        )]
    } else {
        vec![gen_write_16bit_register_microcode!(
            WordRegisters::PC,
            // handle for underflow
            cpu.pc.read().overflowing_add(inst_offset as u16).0
        )]
    };

    // if the branch is true and that branch crosses a page boundary pay a 1 cycle penalty.
    let branch_penalty = match (cond, Page::from(cpu.pc.read()).contains(jmp_on_eq)) {
        (true, false) => 2,
        (true, true) => 1,
        _ => 0,
    };

    MOps::new(0, cycles + branch_penalty, mc)
}

// BCC

gen_instruction_cycles_and_parser!(mnemonic::BCC, addressing_mode::Relative, 0x90, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BCC, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(!cpu.ps.carry, offset, self.offset(), self.cycles(), cpu)
    }
}

// BCS

gen_instruction_cycles_and_parser!(mnemonic::BCS, addressing_mode::Relative, 0xb0, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BCS, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(cpu.ps.carry, offset, self.offset(), self.cycles(), cpu)
    }
}

// BEQ

gen_instruction_cycles_and_parser!(mnemonic::BEQ, addressing_mode::Relative, 0xf0, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BEQ, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(cpu.ps.zero, offset, self.offset(), self.cycles(), cpu)
    }
}

// BMI

gen_instruction_cycles_and_parser!(mnemonic::BMI, addressing_mode::Relative, 0x30, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BMI, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(cpu.ps.negative, offset, self.offset(), self.cycles(), cpu)
    }
}

// BNE

gen_instruction_cycles_and_parser!(mnemonic::BNE, addressing_mode::Relative, 0xd0, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BNE, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(!cpu.ps.zero, offset, self.offset(), self.cycles(), cpu)
    }
}

// BPL

gen_instruction_cycles_and_parser!(mnemonic::BPL, addressing_mode::Relative, 0x10, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BPL, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(!cpu.ps.negative, offset, self.offset(), self.cycles(), cpu)
    }
}

// BVC

gen_instruction_cycles_and_parser!(mnemonic::BVC, addressing_mode::Relative, 0x50, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BVC, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(!cpu.ps.overflow, offset, self.offset(), self.cycles(), cpu)
    }
}

// BVS

gen_instruction_cycles_and_parser!(mnemonic::BVS, addressing_mode::Relative, 0x70, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BVS, addressing_mode::Relative> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let offset = self.addressing_mode.unwrap();

        branch_on_case(cpu.ps.overflow, offset, self.offset(), self.cycles(), cpu)
    }
}

// CLC

gen_instruction_cycles_and_parser!(mnemonic::CLC, addressing_mode::Implied, 0x18, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CLC, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Carry, false)],
        )
    }
}

// CLD

gen_instruction_cycles_and_parser!(mnemonic::CLD, addressing_mode::Implied, 0xd8, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CLD, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Decimal, false)],
        )
    }
}

// CLI

gen_instruction_cycles_and_parser!(mnemonic::CLI, addressing_mode::Implied, 0x58, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CLI, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_flag_set_microcode!(
                ProgramStatusFlags::Interrupt,
                false
            )],
        )
    }
}

// CLV

gen_instruction_cycles_and_parser!(mnemonic::CLV, addressing_mode::Implied, 0xb8, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CLV, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Overflow, false)],
        )
    }
}

// CMP

gen_instruction_cycles_and_parser!(mnemonic::CMP, addressing_mode::Absolute, 0xcd, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);
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

gen_instruction_cycles_and_parser!(
    mnemonic::CMP,
    addressing_mode::AbsoluteIndexedWithX,
    0xdd,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let base_addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(base_addr, index);
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);
        let lhs = Operand::new(cpu.acc.read());
        let carry = lhs >= rhs;
        let diff = lhs - rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(base_addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, diff.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, diff.zero),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::CMP,
    addressing_mode::AbsoluteIndexedWithY,
    0xd9,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let base_addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(base_addr, index);
        let rhs = dereference_address_to_operand(cpu, indexed_addr, 0);
        let lhs = Operand::new(cpu.acc.read());
        let carry = lhs >= rhs;
        let diff = lhs - rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(base_addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, diff.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, diff.zero),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::CMP, addressing_mode::IndirectYIndexed, 0xd1, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let base_addr = self.addressing_mode.unwrap();
        let indirect_addr = dereference_indirect_indexed_address(cpu, base_addr, index);
        let rhs = dereference_address_to_operand(cpu, indirect_addr, 0);
        let lhs = Operand::new(cpu.acc.read());
        let carry = lhs >= rhs;
        let diff = lhs - rhs;

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(base_addr as u16).contains(indirect_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, carry),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, diff.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, diff.zero),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::CMP, addressing_mode::Immediate, 0xc9, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addressing_mode::Immediate(am_value) = self.addressing_mode;
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

gen_instruction_cycles_and_parser!(mnemonic::CMP, addressing_mode::XIndexedIndirect, 0xc1, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), index);
        let rhs = dereference_address_to_operand(cpu, indirect_addr, 0);
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

gen_instruction_cycles_and_parser!(mnemonic::CMP, addressing_mode::ZeroPage, 0xc5, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);
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

gen_instruction_cycles_and_parser!(
    mnemonic::CMP,
    addressing_mode::ZeroPageIndexedWithX,
    0xd5,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CMP, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let base_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let rhs = dereference_address_to_operand(cpu, base_addr, 0);
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

// CPX

gen_instruction_cycles_and_parser!(mnemonic::CPX, addressing_mode::Absolute, 0xec, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CPX, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);
        let lhs = Operand::new(cpu.x.read());
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

gen_instruction_cycles_and_parser!(mnemonic::CPX, addressing_mode::Immediate, 0xe0, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CPX, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addressing_mode::Immediate(am_value) = self.addressing_mode;
        let rhs = Operand::new(am_value);
        let lhs = Operand::new(cpu.x.read());
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

gen_instruction_cycles_and_parser!(mnemonic::CPX, addressing_mode::ZeroPage, 0xe4, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CPX, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);
        let lhs = Operand::new(cpu.x.read());
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

// CPY

gen_instruction_cycles_and_parser!(mnemonic::CPY, addressing_mode::Absolute, 0xcc, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CPY, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap(), 0);
        let lhs = Operand::new(cpu.y.read());
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

gen_instruction_cycles_and_parser!(mnemonic::CPY, addressing_mode::Immediate, 0xc0, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CPY, addressing_mode::Immediate> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addressing_mode::Immediate(am_value) = self.addressing_mode;
        let rhs = Operand::new(am_value);
        let lhs = Operand::new(cpu.y.read());
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

gen_instruction_cycles_and_parser!(mnemonic::CPY, addressing_mode::ZeroPage, 0xc4, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::CPY, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let rhs = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);
        let lhs = Operand::new(cpu.y.read());
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

// DEC

gen_instruction_cycles_and_parser!(mnemonic::DEC, addressing_mode::Absolute, 0xce, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::DEC, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = dereference_address_to_operand(cpu, addr, 0) - Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::DEC,
    addressing_mode::AbsoluteIndexedWithX,
    0xde,
    7
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::DEC, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0) - Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::DEC, addressing_mode::ZeroPage, 0xc6, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::DEC, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let value = dereference_address_to_operand(cpu, addr, 0) - Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::DEC,
    addressing_mode::ZeroPageIndexedWithX,
    0xd6,
    6
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::DEC, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_zeropage_address(addr, index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0) - Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

// DEX

gen_instruction_cycles_and_parser!(mnemonic::DEX, addressing_mode::Implied, 0xca, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::DEX, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.x.read()) - Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_dec_8bit_register_microcode!(ByteRegisters::X, 1),
            ],
        )
    }
}

// DEY

gen_instruction_cycles_and_parser!(mnemonic::DEY, addressing_mode::Implied, 0x88, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::DEY, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.x.read()) - Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_dec_8bit_register_microcode!(ByteRegisters::Y, 1),
            ],
        )
    }
}

// INC

gen_instruction_cycles_and_parser!(mnemonic::INC, addressing_mode::Absolute, 0xee, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::INC, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = dereference_address_to_operand(cpu, addr, 0) + Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::INC,
    addressing_mode::AbsoluteIndexedWithX,
    0xfe,
    7
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::INC, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0) + Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::INC, addressing_mode::ZeroPage, 0xe6, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::INC, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let value = dereference_address_to_operand(cpu, addr, 0) + Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(addr, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::INC,
    addressing_mode::ZeroPageIndexedWithX,
    0xf6,
    6
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::INC, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_zeropage_address(addr, index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0) + Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_memory_microcode!(indexed_addr, value.unwrap()),
            ],
        )
    }
}

// INX

gen_instruction_cycles_and_parser!(mnemonic::INX, addressing_mode::Implied, 0xe8, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::INX, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.x.read()) + Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

// INY

gen_instruction_cycles_and_parser!(mnemonic::INY, addressing_mode::Implied, 0xc8, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::INY, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.y.read()) + Operand::new(1);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, value.unwrap()),
            ],
        )
    }
}

// JMP

gen_instruction_cycles_and_parser!(mnemonic::JMP, addressing_mode::Absolute, 0x4c, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::JMP, addressing_mode::Absolute> {
    fn generate(self, _: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();

        MOps::new(
            0,
            self.cycles(),
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, addr)],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::JMP, addressing_mode::Indirect, 0x6c, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::JMP, addressing_mode::Indirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr = self.addressing_mode.unwrap();
        let lsb = cpu.address_map.read(indirect_addr);
        let msb = cpu.address_map.read(indirect_addr + 1);
        let addr = u16::from_le_bytes([lsb, msb]);

        MOps::new(
            0,
            self.cycles(),
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, addr)],
        )
    }
}

// JSR

gen_instruction_cycles_and_parser!(mnemonic::JSR, addressing_mode::Absolute, 0x20, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::JSR, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();

        // grab the stack pointer and stack pointer - 1 for storing the PC
        let sph: u16 = stack_pointer_from_byte_value(cpu.sp.read());
        let spl: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_sub(1));

        // Add 2 to the program counter and grab as little-endian bytes.
        let [pcl, pch] = cpu.pc.read().wrapping_add(2).to_le_bytes();

        MOps::new(
            0,
            self.cycles(),
            vec![
                gen_write_memory_microcode!(sph, pch),
                gen_dec_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_write_memory_microcode!(spl, pcl),
                gen_dec_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_write_16bit_register_microcode!(WordRegisters::PC, addr),
            ],
        )
    }
}

// LDA

gen_instruction_cycles_and_parser!(mnemonic::LDA, addressing_mode::Immediate, 0xa9, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::Immediate> {
    fn generate(self, _: &MOS6502) -> MOps {
        let value = Operand::new(self.addressing_mode.unwrap());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDA, addressing_mode::ZeroPage, 0xa5, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LDA,
    addressing_mode::ZeroPageIndexedWithX,
    0xb5,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = dereference_address_to_operand(cpu, addr as u16, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDA, addressing_mode::Absolute, 0xad, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addressing_mode::Absolute(addr) = self.addressing_mode;
        let value = Operand::new(cpu.address_map.read(addr));
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LDA,
    addressing_mode::AbsoluteIndexedWithX,
    0xbd,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, addr, index);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LDA,
    addressing_mode::AbsoluteIndexedWithY,
    0xb9,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDA, addressing_mode::IndirectYIndexed, 0xb1, 5);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let zpage_base_addr = self.addressing_mode.unwrap();
        let indirect_addr =
            dereference_indirect_indexed_address(cpu, zpage_base_addr, cpu.y.read());
        let value = Operand::new(cpu.address_map.read(indirect_addr));

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(zpage_base_addr as u16).contains(indirect_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDA, addressing_mode::XIndexedIndirect, 0xa1, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDA, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), cpu.x.read());
        let value = Operand::new(cpu.address_map.read(indirect_addr));

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// LDX

gen_instruction_cycles_and_parser!(mnemonic::LDX, addressing_mode::Absolute, 0xae, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDX, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = dereference_address_to_operand(cpu, addr, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LDX,
    addressing_mode::AbsoluteIndexedWithY,
    0xbe,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDX, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDX, addressing_mode::Immediate, 0xa2, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDX, addressing_mode::Immediate> {
    fn generate(self, _: &MOS6502) -> MOps {
        let value = Operand::new(self.addressing_mode.unwrap());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDX, addressing_mode::ZeroPage, 0xa6, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDX, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LDX,
    addressing_mode::ZeroPageIndexedWithY,
    0xb6,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDX, addressing_mode::ZeroPageIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = dereference_address_to_operand(cpu, addr as u16, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

// LDY

gen_instruction_cycles_and_parser!(mnemonic::LDY, addressing_mode::Absolute, 0xac, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDY, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = dereference_address_to_operand(cpu, addr, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LDY,
    addressing_mode::AbsoluteIndexedWithX,
    0xbc,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDY, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = self.addressing_mode.unwrap();
        let indexed_addr = add_index_to_address(addr, index);
        let value = dereference_address_to_operand(cpu, indexed_addr, 0);

        // if the branch crosses a page boundary pay a 1 cycle penalty.
        let branch_penalty = if !Page::from(addr).contains(indexed_addr) {
            1
        } else {
            0
        };

        MOps::new(
            self.offset(),
            self.cycles() + branch_penalty,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDY, addressing_mode::Immediate, 0xa0, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDY, addressing_mode::Immediate> {
    fn generate(self, _: &MOS6502) -> MOps {
        let value = Operand::new(self.addressing_mode.unwrap());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::LDY, addressing_mode::ZeroPage, 0xa4, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDY, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = dereference_address_to_operand(cpu, self.addressing_mode.unwrap() as u16, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, value.unwrap()),
            ],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::LDY,
    addressing_mode::ZeroPageIndexedWithX,
    0xb4,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::LDY, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = dereference_address_to_operand(cpu, addr as u16, 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, value.unwrap()),
            ],
        )
    }
}

// PHA

gen_instruction_cycles_and_parser!(mnemonic::PHA, addressing_mode::Implied, 0x48, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::PHA, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = cpu.acc.read();
        let sp = cpu.sp.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_write_memory_microcode!(stack_pointer_from_byte_value(sp), value),
                gen_dec_8bit_register_microcode!(ByteRegisters::SP, 1),
            ],
        )
    }
}

// PHP

gen_instruction_cycles_and_parser!(mnemonic::PHP, addressing_mode::Implied, 0x08, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::PHP, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = cpu.ps.read();
        let sp = cpu.sp.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_write_memory_microcode!(stack_pointer_from_byte_value(sp), value),
                gen_dec_8bit_register_microcode!(ByteRegisters::SP, 1),
            ],
        )
    }
}

// PLA

gen_instruction_cycles_and_parser!(mnemonic::PLA, addressing_mode::Implied, 0x68, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::PLA, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let sp = cpu.sp.read().overflowing_add(1).0;
        let value = dereference_address_to_operand(cpu, stack_pointer_from_byte_value(sp), 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// PLP

gen_instruction_cycles_and_parser!(mnemonic::PLP, addressing_mode::Implied, 0x28, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::PLP, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let sp = cpu.sp.read().overflowing_add(1).0;
        let value = dereference_address_to_operand(cpu, stack_pointer_from_byte_value(sp), 0);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_write_8bit_register_microcode!(ByteRegisters::PS, value.unwrap()),
            ],
        )
    }
}

// RTI

gen_instruction_cycles_and_parser!(mnemonic::RTI, addressing_mode::Implied, 0x40, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::RTI, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        // grab the program status
        let sp_psl: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_add(1));
        let sp = cpu.address_map.read(sp_psl);

        // grab the stack pointer and stack pointer - 1 for storing the PC
        let sp_pcl: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_add(2));
        let sp_pch: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_add(3));

        let (lsb, hsb) = (cpu.address_map.read(sp_pcl), cpu.address_map.read(sp_pch));
        let ret_addr = u16::from_le_bytes([lsb, hsb]);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_write_8bit_register_microcode!(ByteRegisters::PS, sp),
                gen_inc_8bit_register_microcode!(ByteRegisters::SP, 2),
                gen_write_16bit_register_microcode!(WordRegisters::PC, ret_addr),
            ],
        )
    }
}

// RTS

gen_instruction_cycles_and_parser!(mnemonic::RTS, addressing_mode::Implied, 0x60, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::RTS, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        // grab the stack pointer and stack pointer - 1 for storing the PC
        let spl: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_add(1));
        let sph: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_add(2));

        let (lsb, hsb) = (cpu.address_map.read(spl), cpu.address_map.read(sph));
        let ret_addr = u16::from_le_bytes([lsb, hsb]);

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::SP, 2),
                gen_write_16bit_register_microcode!(WordRegisters::PC, ret_addr),
            ],
        )
    }
}

// SEC

gen_instruction_cycles_and_parser!(mnemonic::SEC, addressing_mode::Implied, 0x38, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SEC, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Carry, true)],
        )
    }
}

// SED

gen_instruction_cycles_and_parser!(mnemonic::SED, addressing_mode::Implied, 0xf8, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SED, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Decimal, true)],
        )
    }
}

// SEI

gen_instruction_cycles_and_parser!(mnemonic::SEI, addressing_mode::Implied, 0x78, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::SEI, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Interrupt, true)],
        )
    }
}

// STA

gen_instruction_cycles_and_parser!(mnemonic::STA, addressing_mode::Absolute, 0x8d, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addressing_mode::Absolute(addr) = self.addressing_mode;
        let acc_val = cpu.acc.read();
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(addr, acc_val)],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::STA,
    addressing_mode::AbsoluteIndexedWithX,
    0x9d,
    5
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, addressing_mode::AbsoluteIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_address(self.addressing_mode.unwrap(), index);
        let acc_val = cpu.acc.read();
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(indexed_addr, acc_val)],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::STA,
    addressing_mode::AbsoluteIndexedWithY,
    0x99,
    5
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, addressing_mode::AbsoluteIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let indexed_addr = add_index_to_address(self.addressing_mode.unwrap(), index);
        let acc_val = cpu.acc.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(indexed_addr, acc_val)],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::STA, addressing_mode::IndirectYIndexed, 0x91, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, addressing_mode::IndirectYIndexed> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indirect_indexed_address(cpu, self.addressing_mode.unwrap(), cpu.y.read());
        let acc_val = cpu.acc.read();
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(indirect_addr, acc_val)],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::STA, addressing_mode::XIndexedIndirect, 0x81, 6);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, addressing_mode::XIndexedIndirect> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let indirect_addr =
            dereference_indexed_indirect_address(cpu, self.addressing_mode.unwrap(), cpu.x.read());
        let acc_val = cpu.acc.read();
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(indirect_addr, acc_val)],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::STA, addressing_mode::ZeroPage, 0x85, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let acc_val = cpu.acc.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(addr, acc_val)],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::STA,
    addressing_mode::ZeroPageIndexedWithX,
    0x95,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STA, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let acc_val = cpu.acc.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(indexed_addr, acc_val)],
        )
    }
}

// STX

gen_instruction_cycles_and_parser!(mnemonic::STX, addressing_mode::Absolute, 0x8e, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STX, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = cpu.x.read();
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(addr, value)],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::STX, addressing_mode::ZeroPage, 0x86, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STX, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let value = cpu.x.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(addr, value)],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::STX,
    addressing_mode::ZeroPageIndexedWithY,
    0x96,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STX, addressing_mode::ZeroPageIndexedWithY> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.y.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = cpu.x.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(indexed_addr, value)],
        )
    }
}

// STY

gen_instruction_cycles_and_parser!(mnemonic::STY, addressing_mode::Absolute, 0x8c, 4);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STY, addressing_mode::Absolute> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap();
        let value = cpu.y.read();
        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(addr, value)],
        )
    }
}

gen_instruction_cycles_and_parser!(mnemonic::STY, addressing_mode::ZeroPage, 0x84, 3);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STY, addressing_mode::ZeroPage> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let addr = self.addressing_mode.unwrap() as u16;
        let value = cpu.y.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(addr, value)],
        )
    }
}

gen_instruction_cycles_and_parser!(
    mnemonic::STY,
    addressing_mode::ZeroPageIndexedWithX,
    0x94,
    4
);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::STY, addressing_mode::ZeroPageIndexedWithX> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let index = cpu.x.read();
        let indexed_addr = add_index_to_zeropage_address(self.addressing_mode.unwrap(), index);
        let value = cpu.y.read();

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_memory_microcode!(indexed_addr, value)],
        )
    }
}

// TAX

gen_instruction_cycles_and_parser!(mnemonic::TAX, addressing_mode::Implied, 0xaa, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::TAX, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.acc.read());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

// TAY

gen_instruction_cycles_and_parser!(mnemonic::TAY, addressing_mode::Implied, 0xa8, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::TAY, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.acc.read());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, value.unwrap()),
            ],
        )
    }
}

// TSX

gen_instruction_cycles_and_parser!(mnemonic::TSX, addressing_mode::Implied, 0xba, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::TSX, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.sp.read());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::X, value.unwrap()),
            ],
        )
    }
}

// TXA

gen_instruction_cycles_and_parser!(mnemonic::TXA, addressing_mode::Implied, 0x8a, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::TXA, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.x.read());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// TSX

gen_instruction_cycles_and_parser!(mnemonic::TXS, addressing_mode::Implied, 0x9a, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::TXS, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.x.read());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![gen_write_8bit_register_microcode!(
                ByteRegisters::SP,
                value.unwrap()
            )],
        )
    }
}

// TYA

gen_instruction_cycles_and_parser!(mnemonic::TYA, addressing_mode::Implied, 0x98, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::TYA, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let value = Operand::new(cpu.y.read());

        MOps::new(
            self.offset(),
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, value.negative),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, value.zero),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, value.unwrap()),
            ],
        )
    }
}

// Misc

// BRK

gen_instruction_cycles_and_parser!(mnemonic::BRK, addressing_mode::Implied, 0x00, 7);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::BRK, addressing_mode::Implied> {
    fn generate(self, cpu: &MOS6502) -> MOps {
        let ps = cpu.ps.read();

        let sp_pcl: u16 = stack_pointer_from_byte_value(cpu.sp.read());
        let sp_pch: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_sub(1));
        let sp_ps: u16 = stack_pointer_from_byte_value(cpu.sp.read().wrapping_sub(2));

        // Add 1 to the program counter and grab as little-endian bytes.
        let [pcl, pch] = cpu.pc.read().wrapping_add(1).to_le_bytes();

        // Grab IRQ/BRK vector
        let irq_vector = u16::from_le_bytes([
            cpu.address_map.read(IRQ_VECTOR_LL),
            cpu.address_map.read(IRQ_VECTOR_HH),
        ]);

        MOps::new(
            0, // manually modified in the instruction
            self.cycles(),
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Break, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Interrupt, true),
                gen_write_memory_microcode!(sp_pcl, pcl),
                gen_dec_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_write_memory_microcode!(sp_pch, pch),
                gen_dec_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_write_memory_microcode!(sp_ps, ps), // PS Register
                gen_dec_8bit_register_microcode!(ByteRegisters::SP, 1),
                gen_write_16bit_register_microcode!(WordRegisters::PC, irq_vector),
            ],
        )
    }
}

// NOP

gen_instruction_cycles_and_parser!(mnemonic::NOP, addressing_mode::Implied, 0xea, 2);

impl Generate<MOS6502, MOps> for Instruction<mnemonic::NOP, addressing_mode::Implied> {
    fn generate(self, _: &MOS6502) -> MOps {
        MOps::new(self.offset(), self.cycles(), vec![])
    }
}
