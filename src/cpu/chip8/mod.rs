use crate::address_map::{AddressMap, Addressable};
use crate::cpu::{register::Register, StepState, CPU};

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
        reg_type: register::GPRegisters,
        reg: register::GeneralPurpose<u8>,
    ) -> Self {
        match reg_type {
            register::GPRegisters::V0 => self.v0 = reg,
            register::GPRegisters::V1 => self.v1 = reg,
            register::GPRegisters::V2 => self.v2 = reg,
            register::GPRegisters::V3 => self.v3 = reg,
            register::GPRegisters::V4 => self.v4 = reg,
            register::GPRegisters::V5 => self.v5 = reg,
            register::GPRegisters::V6 => self.v6 = reg,
            register::GPRegisters::V7 => self.v7 = reg,
            register::GPRegisters::V8 => self.v8 = reg,
            register::GPRegisters::V9 => self.v9 = reg,
            register::GPRegisters::VA => self.va = reg,
            register::GPRegisters::VB => self.vb = reg,
            register::GPRegisters::VC => self.vc = reg,
            register::GPRegisters::VD => self.vd = reg,
            register::GPRegisters::VE => self.ve = reg,
            register::GPRegisters::VF => self.vf = reg,
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

impl CPU<Chip8> for Chip8 {
    fn run(self, _cycles: usize) -> StepState<Chip8> {
        StepState::new(1, self)
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
            ByteRegisters::GPRegisters(gpr) => {
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
        cpu
    }
}

impl crate::cpu::Execute<Chip8> for microcode::Inc16bitRegister {
    fn execute(self, cpu: Chip8) -> Chip8 {
        cpu
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
