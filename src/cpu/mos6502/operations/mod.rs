extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{ParseResult, Parser};

pub mod address_mode;
pub mod mnemonic;

/// Operation takes a mnemonic
pub struct Operation<M, A>
where
    M: Cyclable + Offset,
    A: Cyclable + Offset,
{
    mnemonic: M,
    address_mode: A,
}

impl<M, A> Operation<M, A>
where
    M: Cyclable + Offset,
    A: Cyclable + Offset,
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
    M: Cyclable + Offset,
    A: Cyclable + Offset,
{
    fn cycles(&self) -> usize {
        self.mnemonic.cycles() + self.address_mode.cycles()
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
