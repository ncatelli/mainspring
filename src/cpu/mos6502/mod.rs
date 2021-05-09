use std::ops::RangeInclusive;

use crate::{
    address_map::{
        memory::{Memory, ReadWrite},
        AddressMap, Addressable,
    },
    cpu::{register::Register, StepState, CPU},
};

#[macro_use]
pub mod microcode;

#[cfg(test)]
mod tests;

// Address vectors
pub const NMI_VECTOR_LL: u16 = 0xfffa;
pub const NMI_VECTOR_HH: u16 = 0xfffb;
pub const RESET_VECTOR_LL: u16 = 0xfffc;
pub const RESET_VECTOR_HH: u16 = 0xfffd;
pub const IRQ_VECTOR_LL: u16 = 0xfffe;
pub const IRQ_VECTOR_HH: u16 = 0xffff;

pub mod register;
use parcel::Parser;
use register::{
    ByteRegisters, GPRegister, GeneralPurpose, ProcessorStatus, ProgramCounter, ProgramStatusFlags,
    StackPointer, WordRegisters,
};

pub mod operations;

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
    /// use mainspring::address_map::{
    ///     memory::{Memory, ReadOnly},
    ///     Addressable,
    /// };
    /// use mainspring::cpu::mos6502::MOS6502;
    ///
    /// let (start_addr, stop_addr) = (0x6000, 0x7000);
    /// let nop_sled = [0xea; 0x7000 - 0x6000].to_vec();
    ///
    /// assert!(
    ///     MOS6502::default()
    ///         .register_address_space(
    ///             start_addr..=stop_addr,
    ///             Memory::<ReadOnly>::new(0x6000, 0x7000).load(nop_sled),
    ///         ).is_ok()
    ///     )
    /// ```
    pub fn register_address_space(
        mut self,
        space: RangeInclusive<u16>,
        addr_space: impl Addressable<u16> + 'static,
    ) -> Result<Self, String> {
        let am = self.address_map;
        self.address_map = am.register(space, Box::new(addr_space))?;

        Ok(self)
    }

    /// emulates the reset process of the CPU.
    pub fn reset(self) -> StepState<Self> {
        let mut cpu = MOS6502::with_addressmap(self.address_map);
        let lsb: u8 = cpu.address_map.read(RESET_VECTOR_LL);
        let msb: u8 = cpu.address_map.read(RESET_VECTOR_HH);

        cpu.pc = ProgramCounter::default().write(u16::from_le_bytes([lsb, msb]));
        cpu.sp = StackPointer::default();
        StepState::new(6, cpu)
    }

    /// emulates the reset process of the CPU, exporting the options as a Operations type
    pub fn reset_as_mops(&self) -> operations::Operations {
        let lsb: u8 = self.address_map.read(RESET_VECTOR_LL);
        let msb: u8 = self.address_map.read(RESET_VECTOR_HH);
        let pc = ProgramCounter::default().write(u16::from_le_bytes([lsb, msb]));

        operations::Operations::new(
            0,
            6,
            vec![
                gen_write_8bit_register_microcode!(
                    ByteRegisters::PS,
                    ProcessorStatus::default().read()
                ),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::SP,
                    StackPointer::default().read()
                ),
                gen_write_16bit_register_microcode!(WordRegisters::PC, pc.read()),
            ],
        )
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
                    0x0000..=0x00FF,
                    Box::new(Memory::<ReadWrite>::new(0x0000, 0x00FF)),
                )
                .unwrap()
                .register(
                    0x0100..=0x01FF,
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
    fn run(self, cycles: usize) -> StepState<MOS6502> {
        let state = self
            .clone()
            .into_iter()
            .map(Into::<Vec<Vec<microcode::Microcode>>>::into)
            .flatten() // flatten instructions to cycles
            .take(cycles)
            .flatten()
            .fold(self, |c, mc| mc.execute(c));
        StepState::from(state)
    }
}

impl CPU<MOS6502> for StepState<MOS6502> {
    fn run(self, cycles: usize) -> StepState<MOS6502> {
        match self {
            StepState::Ready(cpu) => cpu.run(cycles),
            StepState::NotReady(remaining, cpu) if cycles < remaining => {
                StepState::NotReady(0, cpu)
            }
            StepState::NotReady(remaining, cpu) => cpu.run(cycles - remaining),
        }
    }
}

impl IntoIterator for MOS6502 {
    type Item = operations::Operations;
    type IntoIter = MOS6502IntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        MOS6502IntoIterator::new(self)
    }
}

pub struct MOS6502IntoIterator {
    state: MOS6502,
}

impl From<MOS6502IntoIterator> for MOS6502 {
    fn from(src: MOS6502IntoIterator) -> Self {
        src.state
    }
}

impl MOS6502IntoIterator {
    fn new(state: MOS6502) -> Self {
        MOS6502IntoIterator { state }
    }
}

impl Iterator for MOS6502IntoIterator {
    type Item = operations::Operations;

    fn next(&mut self) -> Option<operations::Operations> {
        let pc = self.state.pc.read();
        let opcodes: [u8; 3] = [
            self.state.address_map.read(pc),
            self.state.address_map.read(pc + 1),
            self.state.address_map.read(pc + 2),
        ];

        // Parse correct operation
        let oper = match operations::VariantParser.parse(&opcodes[..]) {
            Ok(parcel::MatchStatus::Match {
                span: _,
                remainder: _,
                inner: op,
            }) => Ok(op),
            _ => Err(format!("No match found for {}", opcodes[0])),
        }
        .unwrap();

        let mops = oper.generate(&self.state);

        // rectify state
        let microcode_steps: Vec<Vec<microcode::Microcode>> = mops.clone().into();
        self.state = microcode_steps
            .iter()
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
            Self::SetProgramStatusFlagState(mc) => mc.execute(cpu),
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

impl Execute<MOS6502> for microcode::SetProgramStatusFlagState {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let mut status = cpu.ps;

        match self.flag {
            ProgramStatusFlags::Negative => status.negative = self.value,
            ProgramStatusFlags::Overflow => status.overflow = self.value,
            ProgramStatusFlags::Break => status.brk = self.value,
            ProgramStatusFlags::Decimal => status.decimal = self.value,
            ProgramStatusFlags::Interrupt => status.interrupt_disable = self.value,
            ProgramStatusFlags::Zero => status.zero = self.value,
            ProgramStatusFlags::Carry => status.carry = self.value,
        };

        cpu.with_ps_register(status)
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
                cpu.with_gp_register(GPRegister::Y, GeneralPurpose::with_value(value))
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
                cpu.with_gp_register(
                    GPRegister::ACC,
                    GeneralPurpose::with_value(old_val.overflowing_add(value).0),
                )
            }
            ByteRegisters::X => {
                let old_val = cpu.x.read();
                cpu.with_gp_register(
                    GPRegister::X,
                    GeneralPurpose::with_value(old_val.overflowing_add(value).0),
                )
            }
            ByteRegisters::Y => {
                let old_val = cpu.y.read();
                cpu.with_gp_register(
                    GPRegister::Y,
                    GeneralPurpose::with_value(old_val.overflowing_add(value).0),
                )
            }
            ByteRegisters::SP => {
                let old_val = cpu.sp.read();
                cpu.with_sp_register(StackPointer::with_value(old_val.overflowing_add(value).0))
            }
            ByteRegisters::PS => {
                let old_val = cpu.ps.read();
                cpu.with_ps_register(ProcessorStatus::with_value(
                    old_val.overflowing_add(value).0,
                ))
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
                cpu.with_gp_register(
                    GPRegister::ACC,
                    GeneralPurpose::with_value(old_val.overflowing_sub(value).0),
                )
            }
            ByteRegisters::X => {
                let old_val = cpu.x.read();
                cpu.with_gp_register(
                    GPRegister::X,
                    GeneralPurpose::with_value(old_val.overflowing_sub(value).0),
                )
            }
            ByteRegisters::Y => {
                let old_val = cpu.y.read();
                cpu.with_gp_register(
                    GPRegister::Y,
                    GeneralPurpose::with_value(old_val.overflowing_sub(value).0),
                )
            }
            ByteRegisters::SP => {
                let old_val = cpu.sp.read();
                cpu.with_sp_register(StackPointer::with_value(old_val.overflowing_sub(value).0))
            }
            ByteRegisters::PS => {
                let old_val = cpu.ps.read();
                cpu.with_ps_register(ProcessorStatus::with_value(
                    old_val.overflowing_sub(value).0,
                ))
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
        let pc = cpu.pc.read().overflowing_add(self.value).0;
        cpu.with_pc_register(ProgramCounter::with_value(pc))
    }
}

impl Execute<MOS6502> for microcode::Dec16bitRegister {
    fn execute(self, cpu: MOS6502) -> MOS6502 {
        let pc = cpu.pc.read().overflowing_sub(self.value).0;
        cpu.with_pc_register(ProgramCounter::with_value(pc))
    }
}
