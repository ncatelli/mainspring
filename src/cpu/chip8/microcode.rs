use crate::cpu::chip8::register::{ByteRegisters, WordRegisters};

/// An Enumerable type to store each microcode operation possible on the
/// CHIP-8 emulator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Microcode {
    WriteMemory(WriteMemory),
    Write8bitRegister(Write8bitRegister),
    Inc8bitRegister(Inc8bitRegister),
    Dec8bitRegister(Dec8bitRegister),
    Write16bitRegister(Write16bitRegister),
    Inc16bitRegister(Inc16bitRegister),
    Dec16bitRegister(Dec16bitRegister),
    PushStack(PushStack),
    PopStack(PopStack),
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

/// Represents an operation that pushes a value onto the stack, incrementing the
/// stack pointer.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PushStack {
    pub value: u8,
}

impl PushStack {
    pub fn new(value: u8) -> Self {
        Self { value }
    }
}

/// Represents an operation that pops a value from the stack, decrementing the
/// stack pointer.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PopStack {
    pub value: u16,
}

impl PopStack {
    pub fn new(value: u16) -> Self {
        Self { value }
    }
}

#[allow(unused_macros)]
macro_rules! gen_write_memory_microcode {
    ($addr:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::WriteMemory(
            $crate::cpu::chip8::microcode::WriteMemory::new($addr, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_write_8bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::Write8bitRegister(
            $crate::cpu::chip8::microcode::Write8bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_inc_8bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::Inc8bitRegister(
            $crate::cpu::chip8::microcode::Inc8bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_dec_8bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::Dec8bitRegister(
            $crate::cpu::chip8::microcode::Dec8bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_write_16bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::Write16bitRegister(
            $crate::cpu::chip8::microcode::Write16bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_inc_16bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::Inc16bitRegister(
            $crate::cpu::chip8::microcode::Inc16bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_dec_16bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::Dec16bitRegister(
            $crate::cpu::chip8::microcode::Dec16bitRegister::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_push_stack_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::PushStack(
            $crate::cpu::chip8::microcode::PushStack::new($reg, $value),
        )
    };
}

#[allow(unused_macros)]
macro_rules! gen_dec_16bit_register_microcode {
    ($reg:expr, $value:expr) => {
        $crate::cpu::chip8::microcode::Microcode::PopStack(
            $crate::cpu::chip8::microcode::PopStack::new($reg, $value),
        )
    };
}
