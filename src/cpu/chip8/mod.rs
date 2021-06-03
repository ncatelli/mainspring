use crate::address_map::{AddressMap, Addressable};
use crate::cpu::Execute;
use crate::cpu::{register::Register, Cpu, StepState};
use parcel::Parser;

use super::ExecuteMut;

mod memory;
mod microcode;
mod operations;
mod register;
mod u12;

/// Represents the address the program counter is set to on chip reset.
const RESET_PC_VECTOR: u16 = 0x200;

/// Chip8 represents a CHIP-8 CPU.
#[derive(Debug, Clone)]
pub struct Chip8 {
    stack: memory::Ring<u16>,
    address_space: AddressMap<u16, u8>,
    dt: register::ClockDecrementing,
    st: register::ClockDecrementing,
    pc: register::ProgramCounter,
    sp: register::StackPointer,
    i: register::GeneralPurpose<u16>,
    gp_registers: [register::GeneralPurpose<u8>; 0xf],
}

impl Chip8 {
    pub fn with_pc_register(mut self, reg: register::ProgramCounter) -> Self {
        self.pc = reg;
        self
    }

    pub fn with_sp_register(mut self, reg: register::StackPointer) -> Self {
        self.sp = reg;
        self
    }

    pub fn with_timer_register(
        mut self,
        reg_type: register::TimerRegisters,
        reg: register::ClockDecrementing,
    ) -> Self {
        match reg_type {
            register::TimerRegisters::Sound => self.st = reg,
            register::TimerRegisters::Delay => self.dt = reg,
        };
        self
    }

    pub fn with_i_register(mut self, reg: register::GeneralPurpose<u16>) -> Self {
        self.i = reg;
        self
    }

    pub fn with_gp_register(
        mut self,
        reg_type: register::GpRegisters,
        reg: register::GeneralPurpose<u8>,
    ) -> Self {
        self.gp_registers[reg_type as usize] = reg;
        self
    }

    /// Provides a convenient method for unwrapping a GpRegister enum to a
    /// corresponding read of it's namesake register.
    pub fn read_gp_register(&self, reg: register::GpRegisters) -> u8 {
        self.gp_registers
            .get(reg as usize)
            .map(|cpu_reg| cpu_reg.read())
            // Should never fail due to bounded array.
            .unwrap()
    }

    /// Resets a cpu to a clean state.
    pub fn reset() -> Self {
        Self::default()
    }
}

impl Default for Chip8 {
    fn default() -> Self {
        type Rom =
            crate::address_map::memory::Memory<crate::address_map::memory::ReadOnly, u16, u8>;
        type Ram =
            crate::address_map::memory::Memory<crate::address_map::memory::ReadWrite, u16, u8>;

        Self {
            stack: memory::Ring::new(16),
            address_space: AddressMap::default()
                .register(0..=0x1ff, Box::new(Rom::new(0, 0x1ff)))
                .unwrap()
                .register(0x200..=0xfff, Box::new(Ram::new(0x200, 0xfff)))
                .unwrap(),
            dt: register::ClockDecrementing::default(),
            st: register::ClockDecrementing::default(),
            pc: register::ProgramCounter::with_value(RESET_PC_VECTOR),
            sp: register::StackPointer::default(),
            i: register::GeneralPurpose::default(),
            gp_registers: [register::GeneralPurpose::default(); 0xf],
        }
    }
}

impl Cpu<Chip8> for Chip8 {
    fn run(self, cycles: usize) -> StepState<Chip8> {
        let state = self
            .clone()
            .into_iter()
            .take(cycles)
            .flatten()
            .fold(self, |c, mc| mc.execute(c));
        StepState::from(state)
    }
}

impl Cpu<Chip8> for StepState<Chip8> {
    fn run(self, cycles: usize) -> StepState<Chip8> {
        // CHIP-8 instructions are all single cycle. It's always ready.
        self.unwrap().run(cycles)
    }
}

impl IntoIterator for Chip8 {
    type Item = Vec<microcode::Microcode>;
    type IntoIter = Chip8IntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        Chip8IntoIterator::new(self)
    }
}

pub struct Chip8IntoIterator {
    state: Chip8,
}

impl From<Chip8IntoIterator> for Chip8 {
    fn from(src: Chip8IntoIterator) -> Self {
        src.state
    }
}

impl Chip8IntoIterator {
    fn new(state: Chip8) -> Self {
        Chip8IntoIterator { state }
    }
}

impl Iterator for Chip8IntoIterator {
    type Item = Vec<microcode::Microcode>;

    fn next(&mut self) -> Option<Vec<microcode::Microcode>> {
        let pc = self.state.pc.read();
        let opcodes: [(usize, u8); 2] = [
            (pc as usize, self.state.address_space.read(pc)),
            ((pc as usize + 1), self.state.address_space.read(pc + 1)),
        ];

        // Parse correct operation
        let ops = match operations::OpcodeVariantParser.parse(&opcodes[..]) {
            Ok(parcel::MatchStatus::Match {
                span: _,
                remainder: _,
                inner: op,
            }) => Ok(op),
            _ => Err(format!(
                "No match found for {:#02x}",
                u16::from_be_bytes([opcodes[0].1, opcodes[1].1])
            )),
        }
        .unwrap();

        let microcode_steps: Vec<microcode::Microcode> = ops
            .generate(&self.state)
            .into_iter()
            .chain(vec![microcode::Microcode::Inc16bitRegister(
                // increment the PC by instruction size.
                microcode::Inc16bitRegister::new(register::WordRegisters::ProgramCounter, 2),
            )])
            .collect();

        self.state = microcode_steps
            .iter()
            .fold(self.state.clone(), |cpu, mc| mc.execute(cpu));

        Some(microcode_steps)
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Microcode {
    fn execute(self, mut cpu: Chip8) -> Chip8 {
        cpu.execute_mut(&self);
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::Microcode> for Chip8 {
    fn execute_mut(&mut self, mc: &microcode::Microcode) {
        match mc {
            microcode::Microcode::WriteMemory(mc) => self.execute_mut(mc),
            microcode::Microcode::Write8bitRegister(mc) => self.execute_mut(mc),
            microcode::Microcode::Inc8bitRegister(mc) => self.execute_mut(mc),
            microcode::Microcode::Dec8bitRegister(mc) => self.execute_mut(mc),
            microcode::Microcode::Write16bitRegister(mc) => self.execute_mut(mc),
            microcode::Microcode::Inc16bitRegister(mc) => self.execute_mut(mc),
            microcode::Microcode::Dec16bitRegister(mc) => self.execute_mut(mc),
            microcode::Microcode::PushStack(mc) => self.execute_mut(mc),
            microcode::Microcode::PopStack(mc) => self.execute_mut(mc),
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::WriteMemory {
    fn execute(self, mut cpu: Chip8) -> Chip8 {
        cpu.execute_mut(&self);
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::WriteMemory> for Chip8 {
    fn execute_mut(&mut self, mc: &microcode::WriteMemory) {
        self.address_space.write(mc.address, mc.value).unwrap();
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Write8bitRegister {
    fn execute(self, mut cpu: Chip8) -> Chip8 {
        cpu.execute_mut(&self);
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::Write8bitRegister> for Chip8 {
    fn execute_mut(&mut self, mc: &microcode::Write8bitRegister) {
        use register::{ByteRegisters, ClockDecrementing, GeneralPurpose, TimerRegisters};

        match mc.register {
            register::ByteRegisters::GpRegisters(gpr) => {
                self.gp_registers[gpr as usize] = GeneralPurpose::with_value(mc.value);
            }
            ByteRegisters::TimerRegisters(TimerRegisters::Delay) => {
                self.dt = ClockDecrementing::with_value(mc.value);
            }
            ByteRegisters::TimerRegisters(TimerRegisters::Sound) => {
                self.st = ClockDecrementing::with_value(mc.value);
            }
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Inc8bitRegister {
    fn execute(self, mut cpu: Chip8) -> Chip8 {
        cpu.execute_mut(&self);
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::Inc8bitRegister> for Chip8 {
    fn execute_mut(&mut self, mc: &microcode::Inc8bitRegister) {
        use register::{ByteRegisters, ClockDecrementing, GeneralPurpose, TimerRegisters};

        match mc.register {
            register::ByteRegisters::GpRegisters(gpr) => {
                let cpu_reg_value = self.read_gp_register(gpr);
                let new_val = cpu_reg_value.wrapping_add(mc.value);
                self.gp_registers[gpr as usize] = GeneralPurpose::with_value(new_val);
            }
            ByteRegisters::TimerRegisters(TimerRegisters::Delay) => {
                let cpu_reg_value = self.dt.read();
                let new_val = cpu_reg_value.wrapping_add(mc.value);
                self.dt = ClockDecrementing::with_value(new_val);
            }
            ByteRegisters::TimerRegisters(TimerRegisters::Sound) => {
                let cpu_reg_value = self.st.read();
                let new_val = cpu_reg_value.wrapping_add(mc.value);
                self.st = ClockDecrementing::with_value(new_val);
            }
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Dec8bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::Dec8bitRegister> for Chip8 {
    fn execute_mut(&mut self, _: &microcode::Dec8bitRegister) {
        ()
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Write16bitRegister {
    fn execute(self, mut cpu: Chip8) -> Chip8 {
        cpu.execute_mut(&self);
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::Write16bitRegister> for Chip8 {
    fn execute_mut(&mut self, mc: &microcode::Write16bitRegister) {
        match mc.register {
            register::WordRegisters::I => {
                self.i = register::GeneralPurpose::with_value(mc.value);
            }
            register::WordRegisters::ProgramCounter => {
                self.pc = register::ProgramCounter::with_value(mc.value);
            }
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Inc16bitRegister {
    fn execute(self, mut cpu: Chip8) -> Chip8 {
        cpu.execute_mut(&self);
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::Inc16bitRegister> for Chip8 {
    fn execute_mut(&mut self, mc: &microcode::Inc16bitRegister) {
        match mc.register {
            register::WordRegisters::I => {
                let i = self.i.read();
                self.i = register::GeneralPurpose::with_value(i.wrapping_add(mc.value));
            }
            register::WordRegisters::ProgramCounter => {
                let pc = self.pc.read();
                self.pc = register::ProgramCounter::with_value(pc.wrapping_add(mc.value));
            }
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Dec16bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::Dec16bitRegister> for Chip8 {
    fn execute_mut(&mut self, _: &microcode::Dec16bitRegister) {
        ()
    }
}

impl crate::cpu::Execute<Chip8> for microcode::PushStack {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::PushStack> for Chip8 {
    fn execute_mut(&mut self, _: &microcode::PushStack) {
        ()
    }
}

impl crate::cpu::Execute<Chip8> for microcode::PopStack {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::ExecuteMut<microcode::PopStack> for Chip8 {
    fn execute_mut(&mut self, _: &microcode::PopStack) {
        ()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu::Cpu;

    #[test]
    fn should_execute_infinitely_on_jump_to_reset_vector() {
        let mut cpu = Chip8::default();
        cpu.address_space.write(0x200, 0x12).unwrap();
        cpu.address_space.write(0x201, 0x00).unwrap();

        assert_eq!(0x200, cpu.pc.read());
        let state = cpu.run(5).unwrap();
        assert_eq!(0x200, state.pc.read())
    }

    #[test]
    fn should_execute_add_immediate() {
        let mut cpu = Chip8::default().with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::with_value(0x05),
        );
        cpu.address_space.write(0x200, 0x70).unwrap();
        cpu.address_space.write(0x201, 0xfa).unwrap();

        let state = cpu.run(1).unwrap();
        assert_eq!(0xff, state.read_gp_register(register::GpRegisters::V0))
    }
}
