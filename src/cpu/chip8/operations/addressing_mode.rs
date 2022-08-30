use crate::cpu::chip8::{register, u12::u12};

pub trait AddressingMode {}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SoundTimerDestTx {
    pub src: register::GpRegisters,
}

impl AddressingMode for SoundTimerDestTx {}

impl SoundTimerDestTx {
    pub fn new(src: register::GpRegisters) -> Self {
        Self { src }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DelayTimerDestTx {
    pub src: register::GpRegisters,
}

impl AddressingMode for DelayTimerDestTx {}

impl DelayTimerDestTx {
    pub fn new(src: register::GpRegisters) -> Self {
        Self { src }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DelayTimerSrcTx {
    pub dest: register::GpRegisters,
}

impl AddressingMode for DelayTimerSrcTx {}

impl DelayTimerSrcTx {
    pub fn new(dest: register::GpRegisters) -> Self {
        Self { dest }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VxIIndirect {
    pub src: register::GpRegisters,
}

impl AddressingMode for VxIIndirect {}

impl VxIIndirect {
    pub fn new(src: register::GpRegisters) -> Self {
        Self { src }
    }
}

impl Default for VxIIndirect {
    fn default() -> Self {
        Self {
            src: register::GpRegisters::V0,
        }
    }
}
