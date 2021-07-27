//! Stores single operations that perform state changes on the cpu these can
//! include write operations to memory or registers and are the basic building
//! blocks for an instruction implementation

use crate::cpu::mos6502::register::{ByteRegisters, ProgramStatusFlags, WordRegisters};

/// An Enumerable type to store each microcode operation possible on the
/// 6502 simulator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Microcode {
    WriteMemory(u16, u8),
    SetProgramStatusFlagState(SetProgramStatusFlagState),
    Write8bitRegister(Write8bitRegister),
    Inc8bitRegister(Inc8bitRegister),
    Dec8bitRegister(Dec8bitRegister),
    Write16bitRegister(Write16bitRegister),
    Inc16bitRegister(Inc16bitRegister),
    Dec16bitRegister(Dec16bitRegister),
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
pub struct SetProgramStatusFlagState {
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
pub struct Write8bitRegister {
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
pub struct Inc8bitRegister {
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
pub struct Dec8bitRegister {
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
pub struct Write16bitRegister {
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
pub struct Inc16bitRegister {
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
pub struct Dec16bitRegister {
    pub register: WordRegisters,
    pub value: u16,
}

impl Dec16bitRegister {
    pub fn new(register: WordRegisters, value: u16) -> Self {
        Self { register, value }
    }
}

#[allow(unused_macros)]
macro_rules! gen_flag_set_microcode {
    ($flag:expr, $value:expr) => {
        $crate::cpu::mos6502::microcode::Microcode::SetProgramStatusFlagState(
            $crate::cpu::mos6502::microcode::SetProgramStatusFlagState::new($flag, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_write_8bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::mos6502::microcode::Microcode::Write8bitRegister(
            $crate::cpu::mos6502::microcode::Write8bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_inc_8bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::mos6502::microcode::Microcode::Inc8bitRegister(
            $crate::cpu::mos6502::microcode::Inc8bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_dec_8bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::mos6502::microcode::Microcode::Dec8bitRegister(
            $crate::cpu::mos6502::microcode::Dec8bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_write_16bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::mos6502::microcode::Microcode::Write16bitRegister(
            $crate::cpu::mos6502::microcode::Write16bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_inc_16bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::mos6502::microcode::Microcode::Inc16bitRegister(
            $crate::cpu::mos6502::microcode::Inc16bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_dec_16bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::mos6502::microcode::Microcode::Dec16bitRegister(
            $crate::cpu::mos6502::microcode::Dec16bitRegister::new($reg, $value),
        )
    };
}
