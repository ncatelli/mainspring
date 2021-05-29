use crate::address_map::{AddressMap, Addressable};
use crate::cpu::{register::Register, Cpu, StepState};
use crate::cpu::{Execute, Generate};
use parcel::Parser;

mod memory;
mod microcode;
mod operations;
mod register;

/// Represents the address the program counter is set to on chip reset.
const RESET_PC_VECTOR: u16 = 0x200;

/// Chip8 represents a CHIP-8 CPU.
#[derive(Debug, Clone)]
pub struct Chip8 {
    stack: memory::Ring<u16>,
    address_space: AddressMap<u16, u8>,
    dt: register::Decrementing,
    st: register::Decrementing,
    pc: register::ProgramCounter,
    sp: register::StackPointer,
    i: register::GeneralPurpose<u16>,
    v0: register::GeneralPurpose<u8>,
    v1: register::GeneralPurpose<u8>,
    v2: register::GeneralPurpose<u8>,
    v3: register::GeneralPurpose<u8>,
    v4: register::GeneralPurpose<u8>,
    v5: register::GeneralPurpose<u8>,
    v6: register::GeneralPurpose<u8>,
    v7: register::GeneralPurpose<u8>,
    v8: register::GeneralPurpose<u8>,
    v9: register::GeneralPurpose<u8>,
    va: register::GeneralPurpose<u8>,
    vb: register::GeneralPurpose<u8>,
    vc: register::GeneralPurpose<u8>,
    vd: register::GeneralPurpose<u8>,
    ve: register::GeneralPurpose<u8>,
    vf: register::GeneralPurpose<u8>,
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
        reg: register::Decrementing,
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
        match reg_type {
            register::GpRegisters::V0 => self.v0 = reg,
            register::GpRegisters::V1 => self.v1 = reg,
            register::GpRegisters::V2 => self.v2 = reg,
            register::GpRegisters::V3 => self.v3 = reg,
            register::GpRegisters::V4 => self.v4 = reg,
            register::GpRegisters::V5 => self.v5 = reg,
            register::GpRegisters::V6 => self.v6 = reg,
            register::GpRegisters::V7 => self.v7 = reg,
            register::GpRegisters::V8 => self.v8 = reg,
            register::GpRegisters::V9 => self.v9 = reg,
            register::GpRegisters::Va => self.va = reg,
            register::GpRegisters::Vb => self.vb = reg,
            register::GpRegisters::Vc => self.vc = reg,
            register::GpRegisters::Vd => self.vd = reg,
            register::GpRegisters::Ve => self.ve = reg,
            register::GpRegisters::Vf => self.vf = reg,
        };
        self
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
            dt: register::Decrementing::default(),
            st: register::Decrementing::default(),
            pc: register::ProgramCounter::with_value(RESET_PC_VECTOR),
            sp: register::StackPointer::default(),
            i: register::GeneralPurpose::default(),
            v0: register::GeneralPurpose::default(),
            v1: register::GeneralPurpose::default(),
            v2: register::GeneralPurpose::default(),
            v3: register::GeneralPurpose::default(),
            v4: register::GeneralPurpose::default(),
            v5: register::GeneralPurpose::default(),
            v6: register::GeneralPurpose::default(),
            v7: register::GeneralPurpose::default(),
            v8: register::GeneralPurpose::default(),
            v9: register::GeneralPurpose::default(),
            va: register::GeneralPurpose::default(),
            vb: register::GeneralPurpose::default(),
            vc: register::GeneralPurpose::default(),
            vd: register::GeneralPurpose::default(),
            ve: register::GeneralPurpose::default(),
            vf: register::GeneralPurpose::default(),
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
        let ops = match operations::opcodes::OpcodeVariantParser.parse(&opcodes[..]) {
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

        let microcode_steps = ops.generate(&self.state);

        self.state = microcode_steps
            .iter()
            .fold(self.state.clone(), |cpu, mc| mc.execute(cpu));

        Some(microcode_steps)
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Microcode {
    fn execute(self, cpu: Chip8) -> Chip8 {
        match self {
            microcode::Microcode::WriteMemory(mc) => mc.execute(cpu),
            microcode::Microcode::Write8bitRegister(mc) => mc.execute(cpu),
            microcode::Microcode::Inc8bitRegister(mc) => mc.execute(cpu),
            microcode::Microcode::Dec8bitRegister(mc) => mc.execute(cpu),
            microcode::Microcode::Write16bitRegister(mc) => mc.execute(cpu),
            microcode::Microcode::Inc16bitRegister(mc) => mc.execute(cpu),
            microcode::Microcode::Dec16bitRegister(mc) => mc.execute(cpu),
            microcode::Microcode::PushStack(mc) => mc.execute(cpu),
            microcode::Microcode::PopStack(mc) => mc.execute(cpu),
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::WriteMemory {
    fn execute(self, mut cpu: Chip8) -> Chip8 {
        cpu.address_space.write(self.address, self.value).unwrap();
        cpu
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Write8bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        use register::{ByteRegisters, Decrementing, GeneralPurpose};

        let new_val = self.value;

        match self.register {
            ByteRegisters::GpRegisters(gpr) => {
                cpu.with_gp_register(gpr, GeneralPurpose::with_value(new_val))
            }
            ByteRegisters::TimerRegisters(tr) => {
                cpu.with_timer_register(tr, Decrementing::with_value(new_val))
            }
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Inc8bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Dec8bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Write16bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        match self.register {
            register::WordRegisters::I => {
                cpu.with_i_register(register::GeneralPurpose::with_value(self.value))
            }
            register::WordRegisters::ProgramCounter => {
                cpu.with_pc_register(register::ProgramCounter::with_value(self.value))
            }
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Inc16bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        match self.register {
            register::WordRegisters::I => {
                let i = cpu.i.read();
                cpu.with_i_register(register::GeneralPurpose::with_value(i + self.value))
            }
            register::WordRegisters::ProgramCounter => {
                let pc = cpu.pc.read();
                cpu.with_pc_register(register::ProgramCounter::with_value(pc + self.value))
            }
        }
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Dec16bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::Execute<Chip8> for microcode::PushStack {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
    }
}

impl crate::cpu::Execute<Chip8> for microcode::PopStack {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
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
}
