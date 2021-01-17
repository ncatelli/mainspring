//! Stores single operations that perform state changes on the cpu these can
//! include write operations to memory or registers and are the basic building
//! blocks for an instruction implementation

use crate::cpu::mos6502::register::{ByteRegisters, WordRegisters};

/// An Enumerable type to store each microcode operation possible on the
/// 6502 emulator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Microcode {
    WriteMemory(WriteMemory),
    Write8bitRegister(Write8bitRegister),
    Write16bitRegister(Write16bitRegister),
    IncPCRegister(IncPCRegister),
}

/// Represents a write of the value to the memory location specified by the
/// address field.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct WriteMemory {
    pub address: u16,
    pub value: u8,
}

impl WriteMemory {
    pub fn new(address: u16, value: u8) -> Self {
        Self { address, value }
    }
}

// 8-bit registers

/// Represents a write of the specified 8-bit value to one of the 8-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Write8bitRegister {
    pub register: ByteRegisters,
    pub value: u8,
}

impl Write8bitRegister {
    pub fn new(register: ByteRegisters, value: u8) -> Self {
        Self { register, value }
    }
}

// 16-bit registers

/// Represents a write of the specified 16-bit value to one of the 16-bit
/// registers as defined by the ByteRegisters value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Write16bitRegister {
    pub register: WordRegisters,
    pub value: u16,
}

impl Write16bitRegister {
    pub fn new(register: WordRegisters, value: u16) -> Self {
        Self { register, value }
    }
}

/// Represents an increment of the ProgramCounter register by the value
/// specified in the instruction.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct IncPCRegister(pub u16);
