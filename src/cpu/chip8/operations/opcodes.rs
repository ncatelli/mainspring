use crate::cpu::chip8::{operations::ToNibbleBytes, register, u12::u12};
use parcel::prelude::v1::*;

fn absolute_addressed_opcode<'a>(opcode: u8) -> impl parcel::Parser<'a, &'a [(usize, u8)], u12> {
    parcel::take_n(parcel::parsers::byte::any_byte(), 2)
        .map(|bytes| [bytes[0].to_be_nibbles(), bytes[1].to_be_nibbles()])
        .predicate(move |[first, _]| first[0] == opcode)
        .map(|[[_, first], [second, third]]| {
            let upper = 0x0f & first;
            let lower = (second << 4) | third;
            u12::new(u16::from_be_bytes([upper, lower]))
        })
}

fn immediate_addressed_opcode<'a>(
    opcode: u8,
) -> impl parcel::Parser<'a, &'a [(usize, u8)], (register::GpRegisters, u8)> {
    parcel::take_n(parcel::parsers::byte::any_byte(), 2)
        .map(|bytes| [bytes[0].to_be_nibbles(), bytes[1].to_be_nibbles()])
        .predicate(move |[first, _]| first[0] == opcode)
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
}

/// Represents all valid opcodes for the CHIP-8 architecture.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpcodeVariant {
    Cls(Cls),
    Ret(Ret),
    Jp(Jp),
    Call(Call),
    AddImmediate(AddImmediate),
}

/// Provides a Parser type for the OpcodeVariant enum. Constructing an
/// OpcodeVariant from a stream of bytes.
pub struct OpcodeVariantParser;

impl<'a> Parser<'a, &'a [(usize, u8)], OpcodeVariant> for OpcodeVariantParser {
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], OpcodeVariant> {
        parcel::one_of(vec![
            Cls::default().map(OpcodeVariant::Cls),
            Ret::default().map(OpcodeVariant::Ret),
            Jp::default().map(OpcodeVariant::Jp),
            Call::default().map(OpcodeVariant::Call),
            AddImmediate::default().map(OpcodeVariant::AddImmediate),
        ])
        .parse(input)
    }
}

/// Clear the display.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Cls;

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Cls> for Cls {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Cls> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xe0])
            .map(|_| Cls)
            .parse(input)
    }
}

impl From<Cls> for u16 {
    fn from(_: Cls) -> Self {
        0x00e0
    }
}

/// Return from a subroutine.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Ret;

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ret> for Ret {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Ret> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xee])
            .map(|_| Ret)
            .parse(input)
    }
}

impl From<Ret> for u16 {
    fn from(_: Ret) -> Self {
        0x00ee
    }
}

/// Jump to location nnn.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Jp(u12);

impl Jp {
    pub fn new(addr: u12) -> Self {
        Self(addr)
    }

    pub fn addr(&self) -> u12 {
        self.0
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Jp> for Jp {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Jp> {
        absolute_addressed_opcode(0x01).map(Jp).parse(input)
    }
}

impl From<Jp> for u16 {
    fn from(src: Jp) -> Self {
        0x1000 | u16::from(src.0)
    }
}

impl From<Jp> for OpcodeVariant {
    fn from(src: Jp) -> Self {
        OpcodeVariant::Jp(src)
    }
}

/// Call subroutine at nnn.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Call(u12);

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Call> for Call {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Call> {
        absolute_addressed_opcode(0x02).map(Call).parse(input)
    }
}

impl From<Call> for u16 {
    fn from(src: Call) -> Self {
        0x2000 | u16::from(src.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AddImmediate {
    register: register::GpRegisters,
    value: u8,
}

impl AddImmediate {
    pub fn new(register: register::GpRegisters, value: u8) -> Self {
        Self { register, value }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], AddImmediate> for AddImmediate {
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], AddImmediate> {
        immediate_addressed_opcode(0x07)
            .map(|(reg, val)| AddImmediate::new(reg, val))
            .parse(input)
    }
}

impl Default for AddImmediate {
    fn default() -> Self {
        Self {
            register: register::GpRegisters::V0,
            value: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_cls_opcode() {
        let input: Vec<(usize, u8)> = 0x00e0u16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Cls
            }),
            Cls.parse(&input[..])
        );
    }

    #[test]
    fn should_parse_ret_opcode() {
        let input: Vec<(usize, u8)> = 0x00eeu16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Ret
            }),
            Ret::default().parse(&input[..])
        );
    }

    #[test]
    fn should_parse_jump_opcode() {
        let input: Vec<(usize, u8)> = 0x1fffu16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Jp(u12::new(0x0fff))
            }),
            Jp::default().parse(&input[..])
        );
    }

    #[test]
    fn should_parse_call_opcode() {
        let input: Vec<(usize, u8)> = 0x2fffu16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Call(u12::new(0x0fff))
            }),
            Call::default().parse(&input[..])
        );
    }

    #[test]
    fn should_parse_add_immediate_opcode() {
        let input: Vec<(usize, u8)> = 0x70ffu16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: AddImmediate::new(register::GpRegisters::V0, 0xff)
            }),
            AddImmediate::default().parse(&input[..])
        );
    }
}
