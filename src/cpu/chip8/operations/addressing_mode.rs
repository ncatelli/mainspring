use super::instruction_as_nibbles;
use crate::cpu::chip8::{register, u12::u12};

/// A placeholder constant error string until a u4 type is implemented. Other
/// assertions are in place so that this should never be encountered.
const NIBBLE_OVERFLOW: &str = "unreachable nibble should be limited to u4.";

/// Returns a u8 representing the input byte with the most significant
/// masked limiting the maximum value to 0x0f.
const fn least_significant_nibble_from_u8(x: u8) -> u8 {
    x & 0x0f
}

pub trait AddressingMode {}

/// Implied represents a type that explicitly implies it's addressing mode
/// through a 2-byte mnemonic code.
/// # Note
/// Implied addressing mode does not implement Parser as the parsing should be
/// defined on the implementing opcode.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Implied;

impl AddressingMode for Implied {}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Absolute(u12);

impl AddressingMode for Absolute {}

impl Absolute {
    #[allow(dead_code)]
    pub fn new(addr: u12) -> Self {
        Self(addr)
    }

    pub fn addr(&self) -> u12 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Immediate {
    pub register: register::GpRegisters,
    pub value: u8,
}

impl AddressingMode for Immediate {}

impl Immediate {
    pub fn new(register: register::GpRegisters, value: u8) -> Self {
        Self { register, value }
    }
}

impl Default for Immediate {
    fn default() -> Self {
        Self {
            register: register::GpRegisters::V0,
            value: 0,
        }
    }
}

/// Represents an operation on the I register indexed by a General-Purpose
/// register.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IRegisterIndexed {
    pub register: register::GpRegisters,
}

impl AddressingMode for IRegisterIndexed {}

impl IRegisterIndexed {
    pub fn new(register: register::GpRegisters) -> Self {
        Self { register }
    }
}

impl Default for IRegisterIndexed {
    fn default() -> Self {
        Self {
            register: register::GpRegisters::V0,
        }
    }
}

/// Represents a register to register general-purpose operation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VxVy {
    /// Represents the first register defined in this address mode. Often
    /// times this will represent a destination register.
    ///
    /// # Example
    ///
    /// `<mnemonic> <first> <second>` or `Add <first> <second>`
    pub first: register::GpRegisters,

    /// Represents the second register defined in this address mode. Often
    /// times this will represent a source register.
    ///
    /// # Example
    ///
    /// `<mnemonic> <first> <second>` or `Add <first> <second>`
    pub second: register::GpRegisters,
}

impl AddressingMode for VxVy {}

impl VxVy {
    pub fn new(src: register::GpRegisters, dest: register::GpRegisters) -> Self {
        Self {
            first: src,
            second: dest,
        }
    }
}

impl Default for VxVy {
    fn default() -> Self {
        Self {
            first: register::GpRegisters::V0,
            second: register::GpRegisters::V0,
        }
    }
}

/// Represents a register to register operation transfering a value from a
/// register to the Sound Timer register.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SoundTimerDestTx {
    pub src: register::GpRegisters,
}

impl AddressingMode for SoundTimerDestTx {}

impl SoundTimerDestTx {
    pub fn new(src: register::GpRegisters) -> Self {
        Self { src }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], SoundTimerDestTx> for SoundTimerDestTx {
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], SoundTimerDestTx> {
        instruction_as_nibbles()
            .map(|[_, first, _, _]| least_significant_nibble_from_u8(first))
            .map(|reg_id| std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW))
            .map(SoundTimerDestTx::new)
            .parse(input)
    }
}

impl Default for SoundTimerDestTx {
    fn default() -> Self {
        Self {
            src: register::GpRegisters::V0,
        }
    }
}

/// Represents a register to register operation transfering a value from a
/// register to the Delay Timer register.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DelayTimerDestTx {
    pub src: register::GpRegisters,
}

impl AddressingMode for DelayTimerDestTx {}

impl DelayTimerDestTx {
    pub fn new(src: register::GpRegisters) -> Self {
        Self { src }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], DelayTimerDestTx> for DelayTimerDestTx {
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], DelayTimerDestTx> {
        instruction_as_nibbles()
            .map(|[_, first, _, _]| least_significant_nibble_from_u8(first))
            .map(|reg_id| std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW))
            .map(DelayTimerDestTx::new)
            .parse(input)
    }
}

impl Default for DelayTimerDestTx {
    fn default() -> Self {
        Self {
            src: register::GpRegisters::V0,
        }
    }
}

/// Represents a register to register operation transfering a value from a
/// register to the Delay Timer register.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DelayTimerSrcTx {
    pub dest: register::GpRegisters,
}

impl AddressingMode for DelayTimerSrcTx {}

impl DelayTimerSrcTx {
    pub fn new(dest: register::GpRegisters) -> Self {
        Self { dest }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], DelayTimerSrcTx> for DelayTimerSrcTx {
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], DelayTimerSrcTx> {
        instruction_as_nibbles()
            .map(|[_, first, _, _]| least_significant_nibble_from_u8(first))
            .map(|reg_id| std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW))
            .map(DelayTimerSrcTx::new)
            .parse(input)
    }
}

impl Default for DelayTimerSrcTx {
    fn default() -> Self {
        Self {
            dest: register::GpRegisters::V0,
        }
    }
}

/// Represents a register to memory operation taking a value in Vx and storing
/// the result in an indirect address stored in register I.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VxIIndirect {
    pub src: register::GpRegisters,
}

impl AddressingMode for VxIIndirect {}

impl VxIIndirect {
    pub fn new(src: register::GpRegisters) -> Self {
        Self { src }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], VxIIndirect> for VxIIndirect {
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], VxIIndirect> {
        instruction_as_nibbles()
            .map(|[_, first, _, _]| least_significant_nibble_from_u8(first))
            .map(|reg_id| std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW))
            .map(VxIIndirect::new)
            .parse(input)
    }
}

impl Default for VxIIndirect {
    fn default() -> Self {
        Self {
            src: register::GpRegisters::V0,
        }
    }
}
