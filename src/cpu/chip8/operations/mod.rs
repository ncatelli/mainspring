use crate::cpu::chip8::{microcode, Chip8};
use crate::cpu::Generate;

mod opcodes;

#[cfg(test)]
mod tests;

impl Generate<Chip8, Vec<microcode::Microcode>> for opcodes::Jp {
    fn generate(self, _: &Chip8) -> Vec<microcode::Microcode> {
        vec![]
    }
}
