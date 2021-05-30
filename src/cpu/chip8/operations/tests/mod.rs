use super::*;
use crate::cpu::chip8::u12::u12;
use crate::cpu::chip8::Chip8;

#[test]
fn should_generate_jump_with_pc_incrementer() {
    let cpu = Chip8::default();
    assert_eq!(
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            0x1fe
        ))],
        opcodes::Jp::new(u12::new(0x200)).generate(&cpu)
    );

    assert_eq!(
        vec![
            Microcode::Write16bitRegister(Write16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                0x1fe
            )),
            Microcode::Inc16bitRegister(Inc16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                2
            ))
        ],
        opcodes::OpcodeVariant::from(opcodes::Jp::new(u12::new(0x200))).generate(&cpu)
    )
}

#[test]
fn should_generate_add_immediate() {
    let cpu = Chip8::default();
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V5),
            0xff
        ))],
        opcodes::AddImmediate::new(register::GpRegisters::V5, 0xff).generate(&cpu)
    );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(register::GpRegisters::V5),
                0xff
            )),
            Microcode::Inc16bitRegister(Inc16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                2
            ))
        ],
        opcodes::OpcodeVariant::from(opcodes::AddImmediate::new(register::GpRegisters::V5, 0xff))
            .generate(&cpu)
    )
}
