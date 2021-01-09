pub mod address_mode;
pub mod mnemonic;

#[derive(Copy, Clone, PartialEq, Debug)]
/// Operation takes a mnemonic
pub struct Operation<M, A> {
    mnemonic: M,
    address_mode: A,
}
