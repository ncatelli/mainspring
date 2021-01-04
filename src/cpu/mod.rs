use crate::address_map::{
    memory::{Memory, ReadWrite},
    AddressMap,
};

#[cfg(test)]
mod tests;

mod register;
use register::{GeneralPurpose, ProcessorStatus, StackPointer};

/// CPU represents the 6502 CPU
#[derive(Debug)]
pub struct CPU {
    address_map: AddressMap<u16>,
    pub acc: GeneralPurpose,
    pub x: GeneralPurpose,
    pub y: GeneralPurpose,
    pub sp: StackPointer,
    pub pc: GeneralPurpose,
    pub ps: ProcessorStatus,
}

impl CPU {
    pub fn new() -> Self {
        Self::default()
    }

    /// instantiates a new cpu with a provided address_map.
    pub fn with_addressmap(am: AddressMap<u16>) -> Self {
        let mut cpu = Self::default();
        cpu.address_map = am;
        cpu
    }

    /// emulates the reset process of the CPU.
    pub fn reset(self) -> Self {
        CPU::with_addressmap(self.address_map)
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self {
            address_map: AddressMap::new()
                .register(
                    0x0100..0x01FF,
                    Box::new(Memory::<ReadWrite>::new(0x0100, 0x01FF)),
                )
                .unwrap(),
            acc: GeneralPurpose::default(),
            x: GeneralPurpose::default(),
            y: GeneralPurpose::default(),
            sp: StackPointer::default(),
            pc: GeneralPurpose::default(),
            ps: ProcessorStatus::default(),
        }
    }
}
