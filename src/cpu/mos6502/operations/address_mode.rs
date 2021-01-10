extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{MatchStatus, ParseResult, Parser};

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
