extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{parsers::byte::any_byte, MatchStatus, ParseResult, Parser};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Accumulator;

/// Implied address address mode. This is signified by no address mode
/// arguments. An example instruction with an implied address mode would be.
/// `nop`
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Implied;

impl Offset for Implied {
    fn offset(&self) -> usize {
        0
    }
}

impl<'a> Parser<'a, &'a [u8], Implied> for Implied {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Implied> {
        Ok(MatchStatus::Match((input, Implied)))
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Immediate(pub u8);

impl Cyclable for Immediate {}
impl Offset for Immediate {}

impl<'a> Parser<'a, &'a [u8], Immediate> for Immediate {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Immediate> {
        any_byte().map(Immediate).parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Absolute(pub u16);

impl Offset for Absolute {
    fn offset(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], Absolute> for Absolute {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Absolute> {
        parcel::take_n(any_byte(), 2)
            .map(|b| Absolute(u16::from_le_bytes([b[0], b[1]])))
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ZeroPage(pub u8);

impl Cyclable for ZeroPage {}
impl Offset for ZeroPage {}

impl<'a> Parser<'a, &'a [u8], ZeroPage> for ZeroPage {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], ZeroPage> {
        any_byte().map(ZeroPage).parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ZeroPageIndexedWithX(pub u8);

impl Cyclable for ZeroPageIndexedWithX {}
impl Offset for ZeroPageIndexedWithX {}

impl<'a> Parser<'a, &'a [u8], ZeroPageIndexedWithX> for ZeroPageIndexedWithX {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], ZeroPageIndexedWithX> {
        any_byte().map(ZeroPageIndexedWithX).parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ZeroPageIndexedWithY(pub u8);

impl Cyclable for ZeroPageIndexedWithY {}
impl Offset for ZeroPageIndexedWithY {}

impl<'a> Parser<'a, &'a [u8], ZeroPageIndexedWithY> for ZeroPageIndexedWithY {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], ZeroPageIndexedWithY> {
        any_byte().map(ZeroPageIndexedWithY).parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Relative(pub i8);

impl Cyclable for Relative {}
impl Offset for Relative {}

impl<'a> Parser<'a, &'a [u8], Relative> for Relative {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Relative> {
        any_byte()
            .map(|b| {
                let offset = unsafe { std::mem::transmute::<u8, i8>(b) };
                Relative(offset)
            })
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Indirect(pub u16);

impl Offset for Indirect {
    fn offset(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], Indirect> for Indirect {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Indirect> {
        parcel::take_n(any_byte(), 2)
            .map(|b| Indirect(u16::from_le_bytes([b[0], b[1]])))
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct AbsoluteIndexedWithX(pub u16);

impl Offset for AbsoluteIndexedWithX {
    fn offset(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], AbsoluteIndexedWithX> for AbsoluteIndexedWithX {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], AbsoluteIndexedWithX> {
        parcel::take_n(any_byte(), 2)
            .map(|b| AbsoluteIndexedWithX(u16::from_le_bytes([b[0], b[1]])))
            .parse(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbsoluteIndexedWithY(u16);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IndexedIndirect(u8);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IndirectIndexed(u8);
