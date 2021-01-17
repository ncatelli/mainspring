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
use register::{
    ByteRegisters, GPRegister, GeneralPurpose, ProcessorStatus, ProgramCounter, StackPointer,
};

pub mod operations;
use operations::Operation;

pub mod microcode;

pub trait Generate<T, U> {
    fn generate(self, cpu: &T) -> U;
}

pub trait Execute<T> {
    fn execute(self, cpu: T) -> T;
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
    #[allow(clippy::field_reassign_with_default)]
    pub fn with_addressmap(am: AddressMap<u16>) -> Self {
        let mut cpu = Self::default();
        cpu.address_map = am;
        cpu
    }

    /// Functions as a wrapper around the `with_addressmap` and `register`
    /// methods in a way that conforms to the builder pattern and facilitates
    /// chainability of the registration. As such this method _can_ fail and
    /// will forward the errors from the above methods in any case that it
    /// fails.
    ///
    /// # Examples
    ///
    /// ```
    /// use mos6502_emulator::address_map::{
    ///     memory::{Memory, ReadOnly},
    ///     Addressable,
    /// };
    /// use mos6502_emulator::cpu::mos6502::MOS6502;
    ///
    /// let (start_addr, stop_addr) = (0x6000, 0x7000);
    /// let nop_sled = [0xea; 0x7000 - 0x6000].to_vec();
    ///
    /// assert!(
    ///     MOS6502::default()
    ///         .register_address_space(
    ///             start_addr..stop_addr,
    ///             Memory::<ReadOnly>::new(0x6000, 0x7000).load(nop_sled),
    ///         ).is_ok()
    ///     )
    /// ```
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
    type Item = operations::MOps;
    type IntoIter = MOS6502IntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        MOS6502IntoIterator::new(self)
    }
}

pub struct MOS6502IntoIterator {
    state: MOS6502,
}

impl MOS6502IntoIterator {
    fn new(state: MOS6502) -> Self {
        MOS6502IntoIterator { state }
    }
}

impl Iterator for MOS6502IntoIterator {
    type Item = operations::MOps;

    fn next(&mut self) -> Option<operations::MOps> {
        let pc = self.state.pc.read();
        let opcodes: [u8; 3] = [
            self.state.address_map.read(pc),
            self.state.address_map.read(pc + 1),
            self.state.address_map.read(pc + 2),
        ];

        // Parse correct operation
        let oper: Operation = TryFrom::try_from(&opcodes).unwrap();
        let mops = oper.generate(&self.state);

        // rectify state
        let microcode_steps: Vec<Vec<microcode::Microcode>> = mops.clone().into();
        self.state = microcode_steps
            .into_iter()
            .flatten()
            .fold(self.state.clone(), |cpu, mc| mc.execute(cpu));

        Some(mops)
    }
}

// microcode execution

impl Execute<MOS6502> for microcode::Microcode {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        match self {
            Self::WriteMemory(mc) => mc.execute(cpu),
            Self::Write8bitRegister(mc) => mc.execute(cpu),
            Self::Inc8bitRegister(mc) => mc.execute(cpu),
            Self::Dec8bitRegister(mc) => mc.execute(cpu),
            Self::Write16bitRegister(mc) => mc.execute(cpu),
            Self::Inc16bitRegister(mc) => mc.execute(cpu),
            Self::Dec16bitRegister(mc) => mc.execute(cpu),
        }
    }
}

impl Execute<MOS6502> for microcode::WriteMemory {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let mut cpu = cpu;
        cpu.address_map.write(self.address, self.value).unwrap();
        cpu
    }
}

impl Execute<MOS6502> for microcode::Write8bitRegister {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let register = self.register;
        let value = self.value;

        match register {
            ByteRegisters::ACC => {
                cpu.with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(value))
            }
            ByteRegisters::X => {
                cpu.with_gp_register(GPRegister::X, GeneralPurpose::with_value(value))
            }
            ByteRegisters::Y => {
                cpu.with_gp_register(GPRegister::X, GeneralPurpose::with_value(value))
            }
            ByteRegisters::SP => cpu.with_sp_register(StackPointer::with_value(value)),
            ByteRegisters::PS => cpu.with_ps_register(ProcessorStatus::with_value(value)),
        }
    }
}

impl Execute<MOS6502> for microcode::Inc8bitRegister {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let register = self.register;
        let value = self.value;

        match register {
            ByteRegisters::ACC => {
                let old_val = cpu.acc.read();
                cpu.with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(old_val + value))
            }
            ByteRegisters::X => {
                let old_val = cpu.x.read();
                cpu.with_gp_register(GPRegister::X, GeneralPurpose::with_value(old_val + value))
            }
            ByteRegisters::Y => {
                let old_val = cpu.y.read();
                cpu.with_gp_register(GPRegister::X, GeneralPurpose::with_value(old_val + value))
            }
            ByteRegisters::SP => {
                let old_val = cpu.sp.read();
                cpu.with_sp_register(StackPointer::with_value(old_val as u8 + value))
            }
            ByteRegisters::PS => {
                let old_val = cpu.ps.read();
                cpu.with_ps_register(ProcessorStatus::with_value(old_val + value))
            }
        }
    }
}
impl Execute<MOS6502> for microcode::Dec8bitRegister {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let register = self.register;
        let value = self.value;

        match register {
            ByteRegisters::ACC => {
                let old_val = cpu.acc.read();
                cpu.with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(old_val - value))
            }
            ByteRegisters::X => {
                let old_val = cpu.x.read();
                cpu.with_gp_register(GPRegister::X, GeneralPurpose::with_value(old_val - value))
            }
            ByteRegisters::Y => {
                let old_val = cpu.y.read();
                cpu.with_gp_register(GPRegister::X, GeneralPurpose::with_value(old_val - value))
            }
            ByteRegisters::SP => {
                let old_val = cpu.sp.read();
                cpu.with_sp_register(StackPointer::with_value(old_val as u8 - value))
            }
            ByteRegisters::PS => {
                let old_val = cpu.ps.read();
                cpu.with_ps_register(ProcessorStatus::with_value(old_val - value))
            }
        }
    }
}
impl Execute<MOS6502> for microcode::Write16bitRegister {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        cpu.with_pc_register(ProgramCounter::with_value(self.value))
    }
}

impl Execute<MOS6502> for microcode::Inc16bitRegister {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let pc = cpu.pc.read() + self.value;
        cpu.with_pc_register(ProgramCounter::with_value(pc))
    }
}

impl Execute<MOS6502> for microcode::Dec16bitRegister {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let pc = cpu.pc.read() - self.value;
        cpu.with_pc_register(ProgramCounter::with_value(pc))
    }
}
