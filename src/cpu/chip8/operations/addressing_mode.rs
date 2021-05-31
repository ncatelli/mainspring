use crate::cpu::chip8::{operations::ToNibbleBytes, register, u12::u12};

pub trait AddressingMode {}

/// Implied represents a type that explicitly implies it's addressing mode through a 2-byte mnemonic code.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Implied;

impl AddressingMode for Implied {}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Implied> for Implied {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Implied> {
        parcel::take_n(parcel::parsers::byte::any_byte(), 2)
            .map(|_| Implied)
            .parse(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Absolute(u12);

impl AddressingMode for Absolute {}

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
                let reg = match upper {
                    0x0 => register::GpRegisters::V0,
                    0x1 => register::GpRegisters::V1,
                    0x2 => register::GpRegisters::V2,
                    0x3 => register::GpRegisters::V3,
                    0x4 => register::GpRegisters::V4,
                    0x5 => register::GpRegisters::V5,
                    0x6 => register::GpRegisters::V6,
                    0x7 => register::GpRegisters::V7,
                    0x8 => register::GpRegisters::V8,
                    0x9 => register::GpRegisters::V9,
                    0xa => register::GpRegisters::Va,
                    0xb => register::GpRegisters::Vb,
                    0xc => register::GpRegisters::Vc,
                    0xd => register::GpRegisters::Vd,
                    0xe => register::GpRegisters::Ve,
                    0xf => register::GpRegisters::Vf,
                    _ => panic!("unreachable nibble should be limited to u4."),
                };

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
