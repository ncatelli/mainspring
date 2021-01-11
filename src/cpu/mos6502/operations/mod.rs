extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{parsers::byte::expect_byte, ParseResult, Parser};
use std::fmt::Debug;

pub mod address_mode;
pub mod mnemonic;

#[cfg(test)]
mod tests;

/// Operation takes a mnemonic and address mode as arguments for sizing
/// and operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Operation<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    mnemonic: M,
    address_mode: A,
}

impl<M, A> Operation<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    pub fn new(mnemonic: M, address_mode: A) -> Self {
        Operation {
            mnemonic,
            address_mode,
        }
    }
}

impl<M, A> Cyclable for Operation<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    fn cycles(&self) -> usize {
        self.mnemonic.cycles() + self.address_mode.cycles()
    }
}

impl<M, A> Offset for Operation<M, A>
where
    M: Cyclable + Offset + Copy + Debug + PartialEq,
    A: Cyclable + Offset + Copy + Debug + PartialEq,
{
    fn offset(&self) -> usize {
        self.mnemonic.offset() + self.address_mode.offset()
    }
}

impl std::convert::TryFrom<&[u8; 3]> for Operation<mnemonic::LDA, address_mode::Immediate> {
    type Error = String;
    fn try_from(values: &[u8; 3]) -> std::result::Result<Self, Self::Error> {
        match Operation::new(mnemonic::LDA, address_mode::Immediate::default()).parse(values) {
            Ok(parcel::MatchStatus::Match((_, op))) => Ok(op),
            _ => Err(format!("No match found for {}", values[0])),
        }
    }
}

impl<'a> Parser<'a, &'a [u8], Operation<mnemonic::LDA, address_mode::Immediate>>
    for Operation<mnemonic::LDA, address_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Operation<mnemonic::LDA, address_mode::Immediate>> {
        expect_byte(0xa9)
            .and_then(|_| address_mode::Immediate::default())
            .map(|am| Operation::new(mnemonic::LDA, am))
            .parse(input)
    }
}

impl std::convert::TryFrom<&[u8; 3]> for Operation<mnemonic::NOP, address_mode::Implied> {
    type Error = String;
    fn try_from(values: &[u8; 3]) -> std::result::Result<Self, Self::Error> {
        match Operation::new(mnemonic::NOP, address_mode::Implied).parse(values) {
            Ok(parcel::MatchStatus::Match((_, op))) => Ok(op),
            _ => Err(format!("No match found for {}", values[0])),
        }
    }
}

impl<'a> Parser<'a, &'a [u8], Operation<mnemonic::NOP, address_mode::Implied>>
    for Operation<mnemonic::NOP, address_mode::Implied>
{
    fn parse(
        &self,
        input: &'a [u8],
    ) -> ParseResult<&'a [u8], Operation<mnemonic::NOP, address_mode::Implied>> {
        mnemonic::NOP
            .and_then(|_| address_mode::Implied)
            .map(|am| Operation::new(mnemonic::NOP, am))
            .parse(input)
    }
}
