use crate::cpu::chip8::{
    self,
    register::{ByteRegisters, WordRegisters},
};

/// An Enumerable type to store each microcode operation possible on the
/// CHIP-8 simulator.
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
    KeyPress(KeyPress),
    KeyRelease,
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
    pub value: u16,
}

impl PushStack {
    pub fn new(value: u16) -> Self {
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

/// Represents a keypress coming in from an external keyboard.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct KeyPress {
    pub value: chip8::InputValues,
}

impl KeyPress {
    pub fn new(value: chip8::InputValues) -> Self {
        Self { value }
    }
}

/// Represents the inverse of KeyPress, a key being released.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct KeyRelease;

impl KeyRelease {
    pub fn new() -> Self {
        Self::default()
    }
}
