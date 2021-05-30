use crate::cpu::chip8::register;
use crate::cpu::chip8::{microcode::*, Chip8};
use crate::cpu::register::Register;
use crate::cpu::Generate;

pub mod opcodes;

#[cfg(test)]
mod tests;

/// ToNibble provides methods for fetching the upper and lower nibble of a byte.
pub trait ToNibble {
    fn to_upper_nibble(&self) -> u8;
    fn to_lower_nibble(&self) -> u8;
}

impl ToNibble for u8 {
    fn to_upper_nibble(&self) -> u8 {
        (self & 0xf0) >> 4
    }

    fn to_lower_nibble(&self) -> u8 {
        self & 0x0f
    }
}

/// ToNibbles defines a trait for converting a type from a value into its
/// corresponding nibbles.
pub trait ToNibbleBytes {
    fn to_be_nibbles(&self) -> [u8; 2];
    fn to_le_nibbles(&self) -> [u8; 2];
}

impl ToNibbleBytes for u8 {
    fn to_be_nibbles(&self) -> [u8; 2] {
        [self.to_upper_nibble(), self.to_lower_nibble()]
    }

    fn to_le_nibbles(&self) -> [u8; 2] {
        [self.to_lower_nibble(), self.to_upper_nibble()]
    }
}

impl Generate<Chip8, Vec<Microcode>> for opcodes::OpcodeVariant {
    fn generate(self, cpu: &Chip8) -> Vec<Microcode> {
        match self {
            opcodes::OpcodeVariant::Jp(op) => Generate::generate(op, cpu),
            opcodes::OpcodeVariant::AddImmediate(op) => Generate::generate(op, cpu),
            // TODO: Empty placeholder representing a NOP
            _ => vec![],
        }
        .into_iter()
        .chain(vec![Microcode::Inc16bitRegister(
            // increment the PC by instruction size.
            Inc16bitRegister::new(register::WordRegisters::ProgramCounter, 2),
        )])
        .collect()
    }
}

impl Generate<Chip8, Vec<Microcode>> for opcodes::Jp {
    fn generate(self, _: &Chip8) -> Vec<Microcode> {
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            u16::from(self.addr()).wrapping_sub(2),
        ))]
    }
}

impl Generate<Chip8, Vec<Microcode>> for opcodes::AddImmediate {
    fn generate(self, cpu: &Chip8) -> Vec<Microcode> {
        let cpu_reg_value = match self.register {
            register::GpRegisters::V0 => cpu.v0.read(),
            register::GpRegisters::V1 => cpu.v1.read(),
            register::GpRegisters::V2 => cpu.v2.read(),
            register::GpRegisters::V3 => cpu.v3.read(),
            register::GpRegisters::V4 => cpu.v4.read(),
            register::GpRegisters::V5 => cpu.v5.read(),
            register::GpRegisters::V6 => cpu.v6.read(),
            register::GpRegisters::V7 => cpu.v7.read(),
            register::GpRegisters::V8 => cpu.v8.read(),
            register::GpRegisters::V9 => cpu.v9.read(),
            register::GpRegisters::Va => cpu.va.read(),
            register::GpRegisters::Vb => cpu.vb.read(),
            register::GpRegisters::Vc => cpu.vc.read(),
            register::GpRegisters::Vd => cpu.vd.read(),
            register::GpRegisters::Ve => cpu.ve.read(),
            register::GpRegisters::Vf => cpu.vf.read(),
        };
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.register),
            cpu_reg_value.wrapping_add(self.value),
        ))]
    }
}
