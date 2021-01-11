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

impl Cyclable for Implied {}

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Immediate(u8);

impl Cyclable for Immediate {}
impl Offset for Immediate {}

impl<'a> Parser<'a, &'a [u8], Immediate> for Immediate {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Immediate> {
        any_byte().map(|b| Immediate(b)).parse(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Absolute(u16);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ZeroPage(u8);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Relative(i8);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Indirect(u16);

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
