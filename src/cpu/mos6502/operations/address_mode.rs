extern crate parcel;
use crate::cpu::Cyclable;
use parcel::{MatchStatus, ParseResult, Parser};

pub struct Accumulator;

/// Implied address address mode. This is signified by no address mode
/// arguments. An example instruction with an implied address mode would be.
/// `nop`
pub struct Implied;

impl Cyclable for Implied {}

impl<'a> Parser<'a, &'a [u8], Implied> for Implied {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Implied> {
        Ok(MatchStatus::Match((input, Implied)))
    }
}

pub struct Immediate(u8);
pub struct Absolute(u16);
pub struct ZeroPage(u8);
pub struct Relative(i8);
pub struct Indirect(u16);
pub struct AbsoluteIndexedWithX(u16);
pub struct AbsoluteIndexedWithY(u16);
pub struct ZeroPageIndexedWithX(u8);
pub struct ZeroPageIndexedWithY(u8);
pub struct IndexedIndirect(u8);
pub struct IndirectIndexed(u8);
