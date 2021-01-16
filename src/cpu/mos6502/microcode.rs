//! Stores single operations that perform state changes on the cpu these can
//! include write operations to memory or registers and are the basic building
//! blocks for an instruction implementation

/// An Enumerable type to store each microcode operation possible on the
/// 6502 emulator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Microcode {
    WriteMemory(WriteMemory),
    WriteAccRegister(WriteAccRegister),
    WriteXRegister(WriteXRegister),
    WriteYRegister(WriteYRegister),
    WritePSRegister(WritePSRegister),
    WriteSPRegister(WriteSPRegister),
    WritePCRegister(WritePCRegister),
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

/// Represents a write of the specified 8-bit value to the A register.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct WriteAccRegister(u8);

/// Represents a write of the specified 8-bit value to the X register.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct WriteXRegister(u8);

/// Represents a write of the specified 8-bit value to the Y register.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct WriteYRegister(u8);

/// Represents a write of the specified 8-bit value to the ProgramStatus
/// register.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct WritePSRegister(u8);

/// Represents a write of the specified 8-bit value to the StackPointer
/// register.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct WriteSPRegister(u8);

// 16-bit registers

/// Represents a write of the specified 16-bit value to the ProgramCounter
/// register.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct WritePCRegister(u16);
