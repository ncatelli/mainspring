extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{parsers::byte::any_byte, MatchStatus, ParseResult, Parser};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Accumulator;

/// Implied address address mode. This is signified by no address mode
/// arguments. An example instruction with an implied address mode would be.
/// `nop`
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Implied;

impl Cyclable for Implied {
    fn cycles(&self) -> usize {
        0
    }
}

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

impl Cyclable for Absolute {
    fn cycles(&self) -> usize {
        2
    }
}
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ZeroPage(u8);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Relative(i8);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Indirect(pub u16);

impl Cyclable for Indirect {
    fn cycles(&self) -> usize {
        4
    }
}
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbsoluteIndexedWithX(u16);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbsoluteIndexedWithY(u16);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ZeroPageIndexedWithX(u8);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ZeroPageIndexedWithY(u8);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IndexedIndirect(u8);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IndirectIndexed(u8);
