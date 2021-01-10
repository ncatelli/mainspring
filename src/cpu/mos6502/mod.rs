extern crate parcel;
use parcel::Parser;

use crate::{
    address_map::{
        memory::{Memory, ReadWrite},
        AddressMap, Addressable,
    },
    cpu::{
        register::{GeneralPurpose, ProcessorStatus, ProgramCounter, Register, StackPointer},
        Cyclable, Offset, StepState, CPU,
    },
};

#[cfg(test)]
mod tests;

pub mod operations;
use operations::{address_mode, mnemonic, Operation};

trait Execute<T> {
    fn execute(self, operation: T) -> Self;
}

/// MOS6502 represents the 6502 CPU
#[derive(Debug)]
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
    pub fn with_addressmap(am: AddressMap<u16>) -> Self {
        let mut cpu = Self::default();
        cpu.address_map = am;
        cpu
    }

    /// emulates the reset process of the CPU.
    pub fn reset(self) -> StepState<Self> {
        let mut cpu = MOS6502::with_addressmap(self.address_map);
        let lsb: u8 = cpu.address_map.read(0x7ffc);
        let msb: u8 = cpu.address_map.read(0x7ffd);

        cpu.pc = ProgramCounter::default().write(u16::from_le_bytes([lsb, msb]));
        StepState::new(6, cpu)
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
            let oper = match opcodes[0] {
                0xEA => Operation::new(mnemonic::NOP, address_mode::Implied).parse(&opcodes),
                _ => Err(format!("unimplemented opcode: {}", opcodes[0])),
            }
            .unwrap()
            .unwrap();

            // run the operation to transform the cpu state
            let mut mos = mos.execute(oper);

            // set pc offsets and cycles as defined by operation.
            let offset = oper.offset() as u16;
            mos.pc = mos.pc.write(pc + offset);
            StepState::new(oper.cycles(), mos)
        }
    }
}

impl Execute<Operation<mnemonic::NOP, address_mode::Implied>> for MOS6502 {
    fn execute(self, _: Operation<mnemonic::NOP, address_mode::Implied>) -> Self {
        self
    }
}
