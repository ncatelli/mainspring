use super::*;
use crate::cpu::chip8::register;
use crate::cpu::chip8::u12::u12;
use crate::cpu::chip8::Chip8;
use crate::cpu::register::Register;

#[test]
fn should_parse_cls_opcode() {
    let input: Vec<(usize, u8)> = 0x00e0u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Cls::default()
        }),
        Cls::default().parse(&input[..])
    );
}

#[test]
fn should_parse_ret_opcode() {
    let input: Vec<(usize, u8)> = 0x00eeu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Ret::default()
        }),
        Ret::default().parse(&input[..])
    );
}

#[test]
fn should_parse_jump_absolute_opcode() {
    let input: Vec<(usize, u8)> = 0x1fffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Jp::<NonV0Indexed, addressing_mode::Absolute>::new(
                addressing_mode::Absolute::new(u12::new(0xfff))
            )
        }),
        Jp::default().parse(&input[..])
    );
}

#[test]
fn should_generate_jump_absolute_with_pc_incrementer() {
    let cpu = Chip8::default();
    assert_eq!(
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            0x1fe
        ))],
        Jp::<NonV0Indexed, addressing_mode::Absolute>::new(addressing_mode::Absolute::new(
            u12::new(0x200)
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_load_absolute_into_i_opcode() {
    let input: Vec<(usize, u8)> = 0xafffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Ld::<addressing_mode::Absolute>::new(addressing_mode::Absolute::new(u12::new(
                0xfff
            )))
        }),
        Ld::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_absolute_into_i_incrementer() {
    let cpu = Chip8::default();
    assert_eq!(
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::I,
            0xfff
        ))],
        Ld::<addressing_mode::Absolute>::new(addressing_mode::Absolute::new(u12::new(0xfff)))
            .generate(&cpu)
    );
}

#[test]
fn should_parse_load_byte_register_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x8010u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Ld::new(addressing_mode::ByteRegisterTx::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Ld<addressing_mode::ByteRegisterTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_byte_register_operation() {
    let cpu = Chip8::default()
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V0),
            0x0f
        ))],
        Ld::new(addressing_mode::ByteRegisterTx::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_load_byte_into_sound_timer_opcode() {
    let input: Vec<(usize, u8)> = 0xF818u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Ld::new(addressing_mode::SoundTimerTx::new(
                register::GpRegisters::V8,
            ))
        }),
        <Ld<addressing_mode::SoundTimerTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_byte_into_sound_timer_operation() {
    let cpu = Chip8::default().with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::TimerRegisters(register::TimerRegisters::Sound),
            0xff
        ))],
        Ld::new(addressing_mode::SoundTimerTx::new(
            register::GpRegisters::V0,
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_jump_absolute_indexed_by_v0_opcode() {
    let input: Vec<(usize, u8)> = 0xbfffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Jp::<V0Indexed, addressing_mode::Absolute>::new(addressing_mode::Absolute::new(
                u12::new(0xfff)
            ))
        }),
        Jp::default().parse(&input[..])
    );
}

#[test]
fn should_generate_jump_absolute_indexed_by_v0_with_pc_incrementer() {
    let cpu = Chip8::default().with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::with_value(0x05),
    );
    assert_eq!(
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            0x203
        ))],
        Jp::<V0Indexed, addressing_mode::Absolute>::new(addressing_mode::Absolute::new(u12::new(
            0x200
        )))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_call_opcode() {
    let input: Vec<(usize, u8)> = 0x2fffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Call::new(addressing_mode::Absolute::new(u12::new(0xfff)))
        }),
        Call::default().parse(&input[..])
    );
}

#[test]
fn should_parse_add_immediate_opcode() {
    let input: Vec<(usize, u8)> = 0x70ffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Add::new(addressing_mode::Immediate::new(
                register::GpRegisters::V0,
                0xff
            ))
        }),
        <Add<addressing_mode::Immediate>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_add_immediate() {
    let cpu = Chip8::default();
    assert_eq!(
        vec![Microcode::Inc8bitRegister(Inc8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V5),
            0xff
        ))],
        Add::new(addressing_mode::Immediate::new(
            register::GpRegisters::V5,
            0xff
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_add_i_register_indexed_opcode() {
    let input: Vec<(usize, u8)> = 0xf01eu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Add::new(addressing_mode::IRegisterIndexed::new(
                register::GpRegisters::V0
            ))
        }),
        <Add<addressing_mode::IRegisterIndexed>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_add_i_register_indexed() {
    let cpu = Chip8::default().with_gp_register(
        register::GpRegisters::V5,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );
    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::I,
            0xff
        ))],
        Add::new(addressing_mode::IRegisterIndexed::new(
            register::GpRegisters::V5
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_and_byte_register_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x8012u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: And::new(addressing_mode::ByteRegisterTx::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <And<addressing_mode::ByteRegisterTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_and_byte_register_operation() {
    let cpu = Chip8::default()
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V0),
            0x0f
        ))],
        And::new(addressing_mode::ByteRegisterTx::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_or_byte_register_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x8011u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Or::new(addressing_mode::ByteRegisterTx::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Or<addressing_mode::ByteRegisterTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_or_byte_register_operation() {
    let cpu = Chip8::default()
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V0),
            0xff
        ))],
        Or::new(addressing_mode::ByteRegisterTx::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_xor_byte_register_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x8013u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Xor::new(addressing_mode::ByteRegisterTx::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Xor<addressing_mode::ByteRegisterTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_xor_byte_register_operation() {
    let cpu = Chip8::default()
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V0),
            0xf0
        ))],
        Xor::new(addressing_mode::ByteRegisterTx::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu)
    );
}
