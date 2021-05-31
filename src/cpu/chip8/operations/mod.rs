use crate::cpu::chip8::register;
use crate::cpu::chip8::{microcode::*, Chip8};
use crate::cpu::Generate;

pub mod addressing_mode;
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

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Opcode<M, A> {
    mnemonic: M,
    addressing_mode: A,
}

impl<'a, M, A> parcel::Parser<'a, &'a [(usize, u8)], Opcode<M, A>> for Opcode<M, A> {
    fn parse(
        &self,
        _input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Opcode<M, A>> {
        todo!()
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
    fn generate(self, _: &Chip8) -> Vec<Microcode> {
        vec![Microcode::Inc8bitRegister(Inc8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.register),
            self.value,
        ))]
    }
}
