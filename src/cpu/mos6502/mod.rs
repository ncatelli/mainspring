extern crate parcel;
use std::convert::TryFrom;
use std::ops::Range;

use crate::{
    address_map::{
        memory::{Memory, ReadWrite},
        AddressMap, Addressable,
    },
    cpu::{register::Register, Cyclable, Offset, StepState, CPU},
};

#[cfg(test)]
mod tests;

pub mod register;
use register::{GeneralPurpose, ProcessorStatus, ProgramCounter, StackPointer};

pub mod operations;
use operations::Operation;

pub trait Execute<T> {
    fn execute(self, cpu: T) -> T;
}

/// Represets each type of general purpose register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GPRegister {
    ACC,
    X,
    Y,
}

/// MOS6502 represents the 6502 CPU
#[derive(Debug, Clone)]
pub struct MOS6502 {
    address_map: AddressMap<u16>,
    pub acc: GeneralPurpose,
    pub x: GeneralPurpose,
    pub y: GeneralPurpose,
    pub sp: StackPointer,
    pub pc: ProgramCounter,
    pub ps: ProcessorStatus,
}

impl MOS6502 {
    pub fn new() -> Self {
        Self::default()
    }

    /// instantiates a new MOS6502 with a provided address_map.
    pub fn with_addressmap(am: AddressMap<u16>) -> Self {
        let mut cpu = Self::default();
        cpu.address_map = am;
        cpu
    }

    /// attempts to wrap address space registration for the sake of chainability.
    pub fn register_address_space(
        mut self,
        space: Range<u16>,
        addr_space: impl Addressable<u16> + 'static,
    ) -> Result<Self, String> {
        let am = self.address_map;
        self.address_map = am.register(space, Box::new(addr_space))?;

        Ok(self)
    }

    /// emulates the reset process of the CPU.
    pub fn reset(self) -> StepState<Self> {
        let mut cpu = MOS6502::with_addressmap(self.address_map);
        let lsb: u8 = cpu.address_map.read(0x7ffc);
        let msb: u8 = cpu.address_map.read(0x7ffd);

        cpu.pc = ProgramCounter::default().write(u16::from_le_bytes([lsb, msb]));
        StepState::new(6, cpu)
    }

    /// Provides a wrapper to update a general-purpose register in a way that
    /// returns the entire cpu after modification.
    pub fn with_gp_register(mut self, reg_type: GPRegister, reg: GeneralPurpose) -> Self {
        match reg_type {
            GPRegister::ACC => self.acc = reg,
            GPRegister::X => self.x = reg,
            GPRegister::Y => self.y = reg,
        };
        self
    }

    /// Provides a wrapper to update the stack-pointer register in a way that
    /// returns the entire cpu after modification.
    pub fn with_sp_register(mut self, reg: StackPointer) -> Self {
        self.sp = reg;
        self
    }

    /// Provides a wrapper to update the program-counter register in a way that
    /// returns the entire cpu after modification.
    pub fn with_pc_register(mut self, reg: ProgramCounter) -> Self {
        self.pc = reg;
        self
    }

    /// Provides a wrapper to update the processor-status register in a way that
    /// returns the entire cpu after modification.
    pub fn with_ps_register(mut self, reg: ProcessorStatus) -> Self {
        self.ps = reg;
        self
    }
}

impl Default for MOS6502 {
    fn default() -> Self {
        Self {
            address_map: AddressMap::new()
                .register(
                    0x0000..0x00FF,
                    Box::new(Memory::<ReadWrite>::new(0x0000, 0x00FF)),
                )
                .unwrap()
                .register(
                    0x0100..0x01FF,
                    Box::new(Memory::<ReadWrite>::new(0x0100, 0x01FF)),
                )
                .unwrap(),
            acc: GeneralPurpose::default(),
            x: GeneralPurpose::default(),
            y: GeneralPurpose::default(),
            sp: StackPointer::default(),
            pc: ProgramCounter::default(),
            ps: ProcessorStatus::default(),
        }
    }
}

impl CPU<MOS6502> for MOS6502 {
    fn step(self) -> StepState<MOS6502> {
        StepState::new(1, self).step()
    }
}

impl CPU<MOS6502> for StepState<MOS6502> {
    fn step(self) -> StepState<MOS6502> {
        if !self.ready() {
            self.decrement()
        } else {
            let mos = self.unwrap();
            let pc = mos.pc.read();
            let opcodes: [u8; 3] = [
                mos.address_map.read(pc),
                mos.address_map.read(pc + 1),
                mos.address_map.read(pc + 2),
            ];

            // Parse correct operation
            let oper: Operation = TryFrom::try_from(&opcodes).unwrap();

            // set pc offsets and cycles as defined by operation.
            let offset = oper.offset() as u16;
            let cycles = oper.cycles();
            let executed_state = oper.execute(mos);
            let espc = executed_state.pc.read();
            StepState::new(
                cycles,
                executed_state.with_pc_register(ProgramCounter::with_value(espc + offset)),
            )
        }
    }
}

impl IntoIterator for MOS6502 {
    type Item = StepState<MOS6502>;
    type IntoIter = CPUIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        let cpu_state = StepState::new(1, self);

        CPUIntoIterator { state: cpu_state }
    }
}

impl IntoIterator for StepState<MOS6502> {
    type Item = StepState<MOS6502>;
    type IntoIter = CPUIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        let cpu_state = self;

        CPUIntoIterator { state: cpu_state }
    }
}

pub struct CPUIntoIterator {
    state: StepState<MOS6502>,
}

impl Iterator for CPUIntoIterator {
    type Item = StepState<MOS6502>;

    fn next(&mut self) -> Option<StepState<MOS6502>> {
        let cpu = self.state.clone();
        let new_state = cpu.step();
        self.state = new_state.clone();

        Some(new_state)
    }
}
