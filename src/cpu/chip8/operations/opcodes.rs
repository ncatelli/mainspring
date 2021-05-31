use crate::cpu::chip8::operations::addressing_mode;
use parcel::prelude::v1::*;

pub fn matches_first_nibble_without_taking_input<'a>(
    opcode: u8,
) -> impl Parser<'a, &'a [(usize, u8)], u8> {
    move |input: &'a [(usize, u8)]| match input.get(0) {
        Some(&(pos, next)) if ((next & 0xf0) >> 4) == opcode => Ok(MatchStatus::Match {
            span: pos..pos + 1,
            remainder: &input[0..],
            inner: opcode,
        }),
        _ => Ok(MatchStatus::NoMatch(input)),
    }
}

/// Represents all valid opcodes for the CHIP-8 architecture.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpcodeVariant {
    Cls(Cls),
    Ret(Ret),
    Jp(Jp<addressing_mode::Absolute>),
    Call(Call<addressing_mode::Absolute>),
    AddImmediate(Add<addressing_mode::Immediate>),
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
            <Jp<addressing_mode::Absolute>>::default().map(OpcodeVariant::Jp),
            Call::default().map(OpcodeVariant::Call),
            <Add<addressing_mode::Immediate>>::default().map(OpcodeVariant::AddImmediate),
        ])
        .parse(input)
    }
}

/// Clear the display.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Cls {
    addressing_mode: addressing_mode::Implied,
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Cls> for Cls {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Cls> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xe0])
            .map(|_| Cls::default())
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
pub struct Ret {
    addressing_mode: addressing_mode::Implied,
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ret> for Ret {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Ret> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xee])
            .map(|_| Ret::default())
            .parse(input)
    }
}

impl From<Ret> for u16 {
    fn from(_: Ret) -> Self {
        0x00ee
    }
}

/// Jp the associated value to the value of the specified register. Setting
/// the register to the sum.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Jp<A> {
    pub addressing_mode: A,
}

impl<A> Jp<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Jp<addressing_mode::Absolute>>
    for Jp<addressing_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Jp<addressing_mode::Absolute>> {
        matches_first_nibble_without_taking_input(0x1)
            .and_then(|_| addressing_mode::Absolute::default())
            .map(Jp::new)
            .parse(input)
    }
}

impl From<Jp<addressing_mode::Absolute>> for OpcodeVariant {
    fn from(src: Jp<addressing_mode::Absolute>) -> Self {
        OpcodeVariant::Jp(src)
    }
}

/// Call subroutine at nnn.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Call<A> {
    pub addressing_mode: A,
}

impl<A> Call<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Call<addressing_mode::Absolute>>
    for Call<addressing_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Call<addressing_mode::Absolute>> {
        matches_first_nibble_without_taking_input(0x2)
            .and_then(|_| addressing_mode::Absolute::default())
            .map(Call::new)
            .parse(input)
    }
}

impl From<Call<addressing_mode::Absolute>> for OpcodeVariant {
    fn from(src: Call<addressing_mode::Absolute>) -> Self {
        OpcodeVariant::Call(src)
    }
}

/// Adds the associated value to the value of the specified register. Setting
/// the register to the sum.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Add<A> {
    pub addressing_mode: A,
}

impl<A> Add<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Add<addressing_mode::Immediate>>
    for Add<addressing_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Add<addressing_mode::Immediate>> {
        matches_first_nibble_without_taking_input(0x7)
            .and_then(|_| addressing_mode::Immediate::default())
            .map(Add::new)
            .parse(input)
    }
}

impl From<Add<addressing_mode::Immediate>> for OpcodeVariant {
    fn from(src: Add<addressing_mode::Immediate>) -> Self {
        OpcodeVariant::AddImmediate(src)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu::chip8::{register, u12::u12};

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
                inner: Cls::default()
            }),
            Cls::default().parse(&input[..])
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
                inner: Ret::default()
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
                inner: Jp::new(addressing_mode::Absolute::new(u12::new(0xfff)))
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
                inner: Call::new(addressing_mode::Absolute::new(u12::new(0xfff)))
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
                inner: Add::new(addressing_mode::Immediate::new(
                    register::GpRegisters::V0,
                    0xff
                ))
            }),
            <Add<addressing_mode::Immediate>>::default().parse(&input[..])
        );
    }
}
