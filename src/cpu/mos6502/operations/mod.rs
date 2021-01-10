extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{ParseResult, Parser};

pub mod address_mode;
pub mod mnemonic;

/// Operation takes a mnemonic and address mode as arguments for sizing
/// and operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Operation<M, A>
where
    M: Cyclable + Offset + Copy,
    A: Cyclable + Offset + Copy,
{
    mnemonic: M,
    address_mode: A,
}

impl<M, A> Operation<M, A>
where
    M: Cyclable + Offset + Copy,
    A: Cyclable + Offset + Copy,
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
    M: Cyclable + Offset + Copy,
    A: Cyclable + Offset + Copy,
{
    fn cycles(&self) -> usize {
        self.mnemonic.cycles() + self.address_mode.cycles()
    }
}

impl<M, A> Offset for Operation<M, A>
where
    M: Cyclable + Offset + Copy,
    A: Cyclable + Offset + Copy,
{
    fn offset(&self) -> usize {
        self.mnemonic.offset() + self.address_mode.offset()
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
