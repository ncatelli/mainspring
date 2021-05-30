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
