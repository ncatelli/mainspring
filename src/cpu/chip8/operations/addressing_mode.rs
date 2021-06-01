use crate::cpu::chip8::{operations::ToNibbleBytes, register, u12::u12};

pub trait AddressingMode {}

/// Implied represents a type that explicitly implies it's addressing mode through a 2-byte mnemonic code.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Implied;

impl AddressingMode for Implied {}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Absolute(u12);

impl AddressingMode for Absolute {}

impl Absolute {
    #[allow(dead_code)]
    pub fn new(addr: u12) -> Self {
        Self(addr)
    }

    pub fn addr(&self) -> u12 {
        self.0
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Absolute> for Absolute {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Absolute> {
        parcel::take_n(parcel::parsers::byte::any_byte(), 2)
            .map(|bytes| [bytes[0].to_be_nibbles(), bytes[1].to_be_nibbles()])
            .map(|[[_, first], [second, third]]| {
                let upper = 0x0f & first;
                let lower = (second << 4) | third;
                u12::new(u16::from_be_bytes([upper, lower]))
            })
            .map(Absolute)
            .parse(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Immediate {
    pub register: register::GpRegisters,
    pub value: u8,
}

impl AddressingMode for Immediate {}

impl Immediate {
    pub fn new(register: register::GpRegisters, value: u8) -> Self {
        Self { register, value }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Immediate> for Immediate {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Immediate> {
        parcel::take_n(parcel::parsers::byte::any_byte(), 2)
            .map(|bytes| [bytes[0].to_be_nibbles(), bytes[1].to_be_nibbles()])
            .map(|[[_, first], [second, third]]| {
                let upper = 0x0f & first;
                let lower = (second << 4) | third;
                let reg = std::convert::TryFrom::<u8>::try_from(upper)
                    .expect("unreachable nibble should be limited to u4.");

                (reg, lower)
            })
            .map(|(register, value)| Immediate::new(register, value))
            .parse(input)
    }
}

impl Default for Immediate {
    fn default() -> Self {
        Self {
            register: register::GpRegisters::V0,
            value: 0,
        }
    }
}

/// Represents an operation on the I register indexed by a General-Purpose
/// register.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IRegisterIndexed {
    pub register: register::GpRegisters,
}

impl AddressingMode for IRegisterIndexed {}

impl IRegisterIndexed {
    pub fn new(register: register::GpRegisters) -> Self {
        Self { register }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], IRegisterIndexed> for IRegisterIndexed {
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], IRegisterIndexed> {
        parcel::take_n(parcel::parsers::byte::any_byte(), 2)
            .map(|bytes| [bytes[0].to_be_nibbles(), bytes[1].to_be_nibbles()])
            .map(|[[_, first], _]| {
                let upper = 0x0f & first;
                std::convert::TryFrom::<u8>::try_from(upper)
                    .expect("unreachable nibble should be limited to u4.")
            })
            .map(IRegisterIndexed::new)
            .parse(input)
    }
}

impl Default for IRegisterIndexed {
    fn default() -> Self {
        Self {
            register: register::GpRegisters::V0,
        }
    }
}
