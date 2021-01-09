use crate::cpu::{Cyclable, Offset};

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
