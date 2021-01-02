use crate::address_map::AddressMap;

#[cfg(test)]
mod tests;

mod register;
use register::{GeneralPurpose, ProcessorStatus};

/// CPU represents the 6502 CPU
#[derive(Debug)]
pub struct CPU {
    address_map: AddressMap<u16>,
    acc: GeneralPurpose,
    x: GeneralPurpose,
    y: GeneralPurpose,
    sp: GeneralPurpose,
    pc: GeneralPurpose,
    ps: ProcessorStatus,
}

impl CPU {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_addressmap(am: AddressMap<u16>) -> Self {
        let mut cpu = Self::default();
        cpu.address_map = am;
        cpu
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self {
            address_map: AddressMap::new(),
            acc: GeneralPurpose::default(),
            x: GeneralPurpose::default(),
            y: GeneralPurpose::default(),
            sp: GeneralPurpose::default(),
            pc: GeneralPurpose::default(),
            ps: ProcessorStatus::default(),
        }
    }
}
