use std::ops::RangeInclusive;

use crate::{
    address_map::{
        memory::{Memory, ReadOnly, ReadWrite},
        AddressMap, Addressable,
    },
    cpu::{register::Register, Cpu, Execute, ExecuteMut, Generate, StepState},
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
    ByteRegisters, GeneralPurpose, GpRegister, ProcessorStatus, ProgramCounter, ProgramStatusFlags,
    StackPointer, WordRegisters,
};

pub mod operations;

/// Provides an alias for the 16bit addressed RW stack.
pub type StackMemory = Memory<ReadWrite, u16, u8>;

/// Provides an alias for the 16bit addressable RW RAM.
pub type Ram = Memory<ReadWrite, u16, u8>;

/// Provides an alias for the 16bit addressable RO ROM.
pub type Rom = Memory<ReadOnly, u16, u8>;

/// Mos6502 represents the 6502 CPU
#[derive(Debug, Clone)]
pub struct Mos6502 {
    pub address_map: AddressMap<u16, u8>,
    pub acc: GeneralPurpose,
    pub x: GeneralPurpose,
    pub y: GeneralPurpose,
    pub sp: StackPointer,
    pub pc: ProgramCounter,
    pub ps: ProcessorStatus,
}

impl Mos6502 {
    pub fn new() -> Self {
        Self::default()
    }

    /// instantiates a new Mos6502 with a provided address_map.
    pub fn with_addressmap(am: AddressMap<u16, u8>) -> Self {
        Self {
            address_map: am,
            ..Default::default()
        }
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
    /// use mainspring::cpu::mos6502::{Mos6502, Rom};
    ///
    /// let (start_addr, stop_addr) = (0x6000, 0x7000);
    /// let nop_sled = [0xea; 0x7000 - 0x6000].to_vec();
    ///
    /// assert!(
    ///     Mos6502::default()
    ///         .register_address_space(
    ///             start_addr..=stop_addr,
    ///             Rom::new(0x6000, 0x7000).load(nop_sled),
    ///         ).is_ok()
    ///     )
    /// ```
    pub fn register_address_space(
        mut self,
        space: RangeInclusive<u16>,
        addr_space: impl Addressable<u16, u8> + 'static,
    ) -> Result<Self, String> {
        let am = self.address_map;
        self.address_map = am.register(space, Box::new(addr_space))?;

        Ok(self)
    }

    /// Simulates the reset process of the CPU.
    pub fn reset(self) -> StepState<Self> {
        let mut cpu = Mos6502::with_addressmap(self.address_map);
        let lsb: u8 = cpu.address_map.read(RESET_VECTOR_LL);
        let msb: u8 = cpu.address_map.read(RESET_VECTOR_HH);

        cpu.pc = ProgramCounter::default().write(u16::from_le_bytes([lsb, msb]));
        cpu.sp = StackPointer::default();
        StepState::new(6, cpu)
    }

    /// Simulates the reset process of the CPU, exporting the options as a Operations type
    pub fn reset_as_mops(&self) -> operations::Operations {
        use microcode::Microcode;

        let lsb: u8 = self.address_map.read(RESET_VECTOR_LL);
        let msb: u8 = self.address_map.read(RESET_VECTOR_HH);
        let pc = ProgramCounter::default().write(u16::from_le_bytes([lsb, msb]));

        operations::Operations::new(
            0,
            6,
            vec![
                Microcode::Write8bitRegister(ByteRegisters::Ps, ProcessorStatus::default().read()),
                Microcode::Write8bitRegister(ByteRegisters::Sp, StackPointer::default().read()),
                Microcode::Write16bitRegister(WordRegisters::Pc, pc.read()),
            ],
        )
    }

    /// Provides a function handler for making changes to the address_map of
    /// an owned CPU returning the modified instance representation.
    pub fn with_owned_address_map<F>(mut self, f: F) -> Self
    where
        F: Fn(AddressMap<u16, u8>) -> AddressMap<u16, u8>,
    {
        use std::mem;

        // replace with a temporary placeholder AddressMap.
        let src_am = mem::take(&mut self.address_map);
        let modified_am = (f)(src_am);
        let _ = mem::replace(&mut self.address_map, modified_am);
        self
    }

    /// Provides a function handler for accessing a borrowed address map..
    pub fn with_address_map<F, B>(&self, f: F) -> B
    where
        F: Fn(&AddressMap<u16, u8>) -> B,
    {
        (f)(&self.address_map)
    }

    /// Provides a function handler for making mutable changes to the address_map
    /// representation.
    pub fn with_address_map_mut<F, B>(&mut self, f: F) -> B
    where
        F: Fn(&mut AddressMap<u16, u8>) -> B,
    {
        (f)(&mut self.address_map)
    }

    /// Provides a wrapper to update a general-purpose register in a way that
    /// returns the entire cpu after modification.
    pub fn with_gp_register(mut self, reg_type: GpRegister, reg: GeneralPurpose) -> Self {
        match reg_type {
            GpRegister::Acc => self.acc = reg,
            GpRegister::X => self.x = reg,
            GpRegister::Y => self.y = reg,
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

impl Default for Mos6502 {
    fn default() -> Self {
        Self {
            address_map: AddressMap::new()
                .register(0x0000..=0x00FF, Box::new(StackMemory::new(0x00, 0xFF)))
                .unwrap()
                .register(0x0100..=0x01FF, Box::new(Ram::new(0x0100, 0x01FF)))
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

impl Cpu<Mos6502> for Mos6502 {
    fn run(self, cycles: usize) -> StepState<Mos6502> {
        let state = self
            .clone()
            .into_iter()
            .flat_map(Into::<Vec<Vec<microcode::Microcode>>>::into)
            .take(cycles)
            .flatten()
            .fold(self, |c, mc| Execute::execute(mc, c));
        StepState::from(state)
    }
}

impl IntoIterator for Mos6502 {
    type Item = operations::Operations;
    type IntoIter = Mos6502IntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        Mos6502IntoIterator::new(self)
    }
}

pub struct Mos6502IntoIterator {
    state: Mos6502,
}

impl From<Mos6502IntoIterator> for Mos6502 {
    fn from(src: Mos6502IntoIterator) -> Self {
        src.state
    }
}

impl Mos6502IntoIterator {
    fn new(state: Mos6502) -> Self {
        Mos6502IntoIterator { state }
    }
}

impl Iterator for Mos6502IntoIterator {
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

        let microcode_steps: Vec<Vec<microcode::Microcode>> = mops.clone().into();
        for mc in microcode_steps.iter().flatten() {
            self.state.execute_mut(mc)
        }

        Some(mops)
    }
}

// microcode execution

// For any implementation of ExecuteMut<M> for a given CPU Execute is implemented.
impl<M> crate::cpu::Execute<Mos6502> for M
where
    Mos6502: ExecuteMut<M>,
{
    fn execute(self, mut cpu: Mos6502) -> Mos6502 {
        cpu.execute_mut(&self);
        cpu
    }
}

impl ExecuteMut<microcode::Microcode> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::Microcode) {
        use microcode::*;

        match mc {
            microcode::Microcode::WriteMemory(addr, value) => {
                self.execute_mut(&WriteMemory::new(*addr, *value))
            }
            microcode::Microcode::SetProgramStatusFlagState(flag, value) => {
                self.execute_mut(&SetProgramStatusFlagState::new(*flag, *value))
            }
            microcode::Microcode::Write8bitRegister(register, value) => {
                self.execute_mut(&Write8bitRegister::new(*register, *value))
            }
            microcode::Microcode::Inc8bitRegister(register, value) => {
                self.execute_mut(&Inc8bitRegister::new(*register, *value))
            }
            microcode::Microcode::Dec8bitRegister(register, value) => {
                self.execute_mut(&Dec8bitRegister::new(*register, *value))
            }
            microcode::Microcode::Write16bitRegister(register, value) => {
                self.execute_mut(&Write16bitRegister::new(*register, *value))
            }
            microcode::Microcode::Inc16bitRegister(register, value) => {
                self.execute_mut(&Inc16bitRegister::new(*register, *value))
            }
            microcode::Microcode::Dec16bitRegister(register, value) => {
                self.execute_mut(&Dec16bitRegister::new(*register, *value))
            }
        }
    }
}

impl ExecuteMut<microcode::WriteMemory> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::WriteMemory) {
        self.address_map.write(mc.address, mc.value).unwrap();
    }
}

impl ExecuteMut<microcode::SetProgramStatusFlagState> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::SetProgramStatusFlagState) {
        let mut status = self.ps;

        match mc.flag {
            ProgramStatusFlags::Negative => status.negative = mc.value,
            ProgramStatusFlags::Overflow => status.overflow = mc.value,
            ProgramStatusFlags::Break => status.brk = mc.value,
            ProgramStatusFlags::Decimal => status.decimal = mc.value,
            ProgramStatusFlags::Interrupt => status.interrupt_disable = mc.value,
            ProgramStatusFlags::Zero => status.zero = mc.value,
            ProgramStatusFlags::Carry => status.carry = mc.value,
        };

        self.ps = status;
    }
}

impl ExecuteMut<microcode::Write8bitRegister> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::Write8bitRegister) {
        let register = mc.register;
        let value = mc.value;

        match register {
            ByteRegisters::Acc => {
                self.acc = GeneralPurpose::with_value(value);
            }
            ByteRegisters::X => {
                self.x = GeneralPurpose::with_value(value);
            }
            ByteRegisters::Y => {
                self.y = GeneralPurpose::with_value(value);
            }
            ByteRegisters::Sp => self.sp = StackPointer::with_value(value),
            ByteRegisters::Ps => self.ps = ProcessorStatus::with_value(value),
        }
    }
}

impl ExecuteMut<microcode::Inc8bitRegister> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::Inc8bitRegister) {
        let register = mc.register;
        let value = mc.value;

        match register {
            ByteRegisters::Acc => {
                let old_val = self.acc.read();
                self.acc = GeneralPurpose::with_value(old_val.overflowing_add(value).0);
            }
            ByteRegisters::X => {
                let old_val = self.x.read();
                self.x = GeneralPurpose::with_value(old_val.overflowing_add(value).0);
            }
            ByteRegisters::Y => {
                let old_val = self.y.read();
                self.y = GeneralPurpose::with_value(old_val.overflowing_add(value).0);
            }
            ByteRegisters::Sp => {
                let old_val = self.sp.read();
                self.sp = StackPointer::with_value(old_val.overflowing_add(value).0);
            }
            ByteRegisters::Ps => {
                let old_val = self.ps.read();
                self.ps = ProcessorStatus::with_value(old_val.overflowing_add(value).0);
            }
        }
    }
}

impl ExecuteMut<microcode::Dec8bitRegister> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::Dec8bitRegister) {
        let register = mc.register;
        let value = mc.value;

        match register {
            ByteRegisters::Acc => {
                let old_val = self.acc.read();
                self.acc = GeneralPurpose::with_value(old_val.overflowing_sub(value).0);
            }
            ByteRegisters::X => {
                let old_val = self.x.read();
                self.x = GeneralPurpose::with_value(old_val.overflowing_sub(value).0);
            }
            ByteRegisters::Y => {
                let old_val = self.y.read();
                self.y = GeneralPurpose::with_value(old_val.overflowing_sub(value).0);
            }
            ByteRegisters::Sp => {
                let old_val = self.sp.read();
                self.sp = StackPointer::with_value(old_val.overflowing_sub(value).0);
            }
            ByteRegisters::Ps => {
                let old_val = self.ps.read();
                self.ps = ProcessorStatus::with_value(old_val.overflowing_sub(value).0);
            }
        }
    }
}

impl ExecuteMut<microcode::Write16bitRegister> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::Write16bitRegister) {
        self.pc = ProgramCounter::with_value(mc.value);
    }
}

impl ExecuteMut<microcode::Inc16bitRegister> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::Inc16bitRegister) {
        let pc = self.pc.read().overflowing_add(mc.value).0;
        self.pc = ProgramCounter::with_value(pc);
    }
}

impl ExecuteMut<microcode::Dec16bitRegister> for Mos6502 {
    fn execute_mut(&mut self, mc: &microcode::Dec16bitRegister) {
        let pc = self.pc.read().overflowing_sub(mc.value).0;
        self.pc = ProgramCounter::with_value(pc);
    }
}
