use parcel::Parser;

use crate::cpu::chip8::register;
use crate::cpu::chip8::{microcode::*, Chip8};
use crate::cpu::register::Register;
use crate::cpu::Generate;

pub mod opcodes;

#[cfg(test)]
mod tests;

impl Generate<Chip8, Vec<Microcode>> for opcodes::OpcodeVariant {
    fn generate(self, cpu: &Chip8) -> Vec<Microcode> {
        match self {
            opcodes::OpcodeVariant::Jp(op) => Generate::generate(op, cpu),
            // TODO: Empty placeholder representing a NOP
            _ => vec![],
        }
        .into_iter()
        .chain(vec![Microcode::Write16bitRegister(
            // increment the PC by instruction size.
            Write16bitRegister::new(register::WordRegisters::ProgramCounter, cpu.pc.read() + 2),
        )])
        .collect()
    }
}

impl Generate<Chip8, Vec<Microcode>> for opcodes::Jp {
    fn generate(self, _: &Chip8) -> Vec<Microcode> {
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            self.addr(),
        ))]
    }
}
