use crate::cpu::chip8::{
    self,
    register::{ByteRegisters, WordRegisters},
};

/// An Enumerable type to store each microcode operation possible on the
/// CHIP-8 simulator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Microcode {
    WriteMemory(u16, u8),
    Write8bitRegister(ByteRegisters, u8),
    Inc8bitRegister(ByteRegisters, u8),
    Dec8bitRegister(ByteRegisters, u8),
    Write16bitRegister(WordRegisters, u16),
    Inc16bitRegister(WordRegisters, u16),
    Dec16bitRegister(WordRegisters, u16),
    PushStack(u16),
    PopStack(u16),
    KeyPress(chip8::KeyInputValue),
    KeyRelease,
    SetDisplayPixel(CartesianCoordinate, bool),
    SetDisplayRange(CartesianCoordinate, CartesianCoordinate, bool),
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

/// Represents an operation that pushes a value onto the stack, incrementing the
/// stack pointer.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct PushStack {
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
pub(crate) struct PopStack {
    pub value: u16,
}

impl PopStack {
    pub fn new(value: u16) -> Self {
        Self { value }
    }
}

/// Represents a keypress coming in from an external keyboard.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct KeyPress {
    pub value: chip8::KeyInputValue,
}

impl KeyPress {
    pub fn new(value: chip8::KeyInputValue) -> Self {
        Self { value }
    }
}

/// Represents the inverse of KeyPress, a key being released.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub(crate) struct KeyRelease;

/// Represents an (x, y) cartesian coordinate.
type CartesianCoordinate = (usize, usize);

/// SetDisplayPixel takes a cartesian coordinate representing a pixel and the
/// value to set that pixel to.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub(crate) struct SetDisplayPixel(pub CartesianCoordinate, pub bool);

impl SetDisplayPixel {
    pub fn new(coord: CartesianCoordinate, value: bool) -> Self {
        Self(coord, value)
    }
}

/// SetDisplayRange takes a value representing what to set all pixels that fall
/// within the bounds of the enclosed start and end cartesian coordinates.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub(crate) struct SetDisplayRange {
    pub start: CartesianCoordinate,
    pub end: CartesianCoordinate,
    pub value: bool,
}

impl SetDisplayRange {
    pub fn new(start: CartesianCoordinate, end: CartesianCoordinate, value: bool) -> Self {
        Self { start, end, value }
    }
}
