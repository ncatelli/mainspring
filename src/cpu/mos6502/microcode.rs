//! Stores single operations that perform state changes on the cpu these can
//! include write operations to memory or registers and are the basic building
//! blocks for an instruction implementation

use crate::cpu::mos6502::register::{ByteRegisters, ProgramStatusFlags, WordRegisters};

/// An Enumerable type to store each microcode operation possible on the
/// 6502 simulator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Microcode {
    /// Represents a write of the value to the memory location specified by the
    /// address field.
    WriteMemory(u16, u8),
    /// Represents a write of the value to the memory location specified by the
    /// address field.
    SetProgramStatusFlagState(ProgramStatusFlags, bool),
    /// Represents a write of the specified 8-bit value to one of the 8-bit
    /// registers as defined by the ByteRegisters value.
    Write8bitRegister(ByteRegisters, u8),
    /// Represents an increment of the specified 8-bit value to one of the 8-bit
    /// registers as defined by the ByteRegisters value.
    Inc8bitRegister(ByteRegisters, u8),
    /// Represents an decrement of the specified 8-bit value to one of the 8-bit
    /// registers as defined by the ByteRegisters value.
    Dec8bitRegister(ByteRegisters, u8),
    /// Represents a write of the specified 16-bit value to one of the 16-bit
    /// registers as defined by the ByteRegisters value.
    Write16bitRegister(WordRegisters, u16),
    /// Represents an increment of the specified 16-bit value to one of the 16-bit
    /// registers as defined by the ByteRegisters value.
    Inc16bitRegister(WordRegisters, u16),
    /// Represents an decrement of the specified 16-bit value to one of the 16-bit
    /// registers as defined by the ByteRegisters value.
    Dec16bitRegister(WordRegisters, u16),
}

/// Represents a write of the value to the memory location specified by the
/// address field.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub(crate) struct WriteMemory {
    pub address: u16,
    pub value: u8,
}

impl WriteMemory {
    pub fn new(address: u16, value: u8) -> Self {
        Self { address, value }
    }
}

/// Represents a write of the value to the memory location specified by the
/// address field.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct SetProgramStatusFlagState {
    pub flag: ProgramStatusFlags,
    pub value: bool,
}

impl SetProgramStatusFlagState {
    pub fn new(flag: ProgramStatusFlags, value: bool) -> Self {
        Self { flag, value }
    }
}

// 8-bit registers

/// Represents a write of the specified 8-bit value to one of the 8-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Write8bitRegister {
    pub register: ByteRegisters,
    pub value: u8,
}

impl Write8bitRegister {
    pub fn new(register: ByteRegisters, value: u8) -> Self {
        Self { register, value }
    }
}

/// Represents an increment of the specified 8-bit value to one of the 8-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Inc8bitRegister {
    pub register: ByteRegisters,
    pub value: u8,
}

impl Inc8bitRegister {
    pub fn new(register: ByteRegisters, value: u8) -> Self {
        Self { register, value }
    }
}

/// Represents an decrement of the specified 8-bit value to one of the 8-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Dec8bitRegister {
    pub register: ByteRegisters,
    pub value: u8,
}

impl Dec8bitRegister {
    pub fn new(register: ByteRegisters, value: u8) -> Self {
        Self { register, value }
    }
}

// 16-bit registers

/// Represents a write of the specified 16-bit value to one of the 16-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Write16bitRegister {
    pub register: WordRegisters,
    pub value: u16,
}

impl Write16bitRegister {
    pub fn new(register: WordRegisters, value: u16) -> Self {
        Self { register, value }
    }
}

/// Represents an increment of the specified 16-bit value to one of the 16-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Inc16bitRegister {
    pub register: WordRegisters,
    pub value: u16,
}

impl Inc16bitRegister {
    pub fn new(register: WordRegisters, value: u16) -> Self {
        Self { register, value }
    }
}

/// Represents an decrement of the specified 16-bit value to one of the 16-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Dec16bitRegister {
    pub register: WordRegisters,
    pub value: u16,
}

impl Dec16bitRegister {
    pub fn new(register: WordRegisters, value: u16) -> Self {
        Self { register, value }
    }
}
