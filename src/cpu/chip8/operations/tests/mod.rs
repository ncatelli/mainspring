use super::*;
use crate::cpu::chip8::{self, register, u12::u12, Chip8};
use crate::cpu::register::Register;

fn inst_to_enumerated_be_byte_vec(inst: u16) -> Vec<(usize, u8)> {
    inst.to_be_bytes().iter().copied().enumerate().collect()
}

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
fn should_generate_ret_implied_instruction() {
    let mut cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_pc_register(register::ProgramCounter::with_value(0x200));

    cpu.stack.write(0x0, 0x200);

    assert_eq!(
        vec![
            // save initial value
            Microcode::PopStack(PopStack::new(0x200)),
            // jump to absolute value - 2.
            Microcode::Write16bitRegister(Write16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                0x1fe
            ))
        ],
        Ret::default().generate(&cpu)
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
    let cpu = Chip8::<()>::default().with_rng(|| 0u8);
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
    let cpu = Chip8::<()>::default().with_rng(|| 0u8);
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
fn should_parse_load_immediate_into_i_opcode() {
    let input: Vec<(usize, u8)> = 0x68ffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Ld::<addressing_mode::Immediate>::new(addressing_mode::Immediate::new(
                register::GpRegisters::V8,
                0xff
            ))
        }),
        Ld::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_immediate_into_i_incrementer() {
    let cpu = Chip8::<()>::default().with_rng(|| 0u8);
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V8),
            0xff
        ))],
        Ld::<addressing_mode::Immediate>::new(addressing_mode::Immediate::new(
            register::GpRegisters::V8,
            0xff
        ))
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
            inner: Ld::new(addressing_mode::VxVy::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Ld<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_byte_register_operation() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
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
        Ld::new(addressing_mode::VxVy::new(
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
            inner: Ld::new(addressing_mode::SoundTimerDestTx::new(
                register::GpRegisters::V8,
            ))
        }),
        <Ld<addressing_mode::SoundTimerDestTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_byte_into_sound_timer_operation() {
    let cpu = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::TimerRegisters(register::TimerRegisters::Sound),
            0xff
        ))],
        Ld::new(addressing_mode::SoundTimerDestTx::new(
            register::GpRegisters::V0,
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_load_byte_into_delay_timer_opcode() {
    let input: Vec<(usize, u8)> = 0xF815u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Ld::new(addressing_mode::DelayTimerDestTx::new(
                register::GpRegisters::V8,
            ))
        }),
        <Ld<addressing_mode::DelayTimerDestTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_byte_into_delay_timer_operation() {
    let cpu = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::TimerRegisters(register::TimerRegisters::Delay),
            0xff
        ))],
        Ld::new(addressing_mode::DelayTimerDestTx::new(
            register::GpRegisters::V0,
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_load_byte_into_register_from_delay_timer_opcode() {
    let input: Vec<(usize, u8)> = 0xF807u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Ld::new(addressing_mode::DelayTimerSrcTx::new(
                register::GpRegisters::V8,
            ))
        }),
        <Ld<addressing_mode::DelayTimerSrcTx>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_byte_into_register_from_delay_timer_operation() {
    let cpu = Chip8::<()>::default().with_rng(|| 0u8).with_timer_register(
        register::TimerRegisters::Delay,
        register::ClockDecrementing::with_value(0xff),
    );
    assert_eq!(
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(register::GpRegisters::V0),
            0xff
        ))],
        Ld::new(addressing_mode::DelayTimerSrcTx::new(
            register::GpRegisters::V0,
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_load_bcd_from_vx_i_indirect_operation() {
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
            inner: LdBcd::new(addressing_mode::VxIIndirect::new(register::GpRegisters::V8,))
        }),
        <LdBcd<addressing_mode::VxIIndirect>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_load_bcd_from_vx_i_indirect_operation() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xfe),
        )
        .with_i_register(register::GeneralPurpose::<u16>::with_value(0x0100));

    assert_eq!(
        vec![
            Microcode::WriteMemory(WriteMemory::new(0x0100, 2)),
            Microcode::WriteMemory(WriteMemory::new(0x0101, 5)),
            Microcode::WriteMemory(WriteMemory::new(0x0102, 4)),
        ],
        LdBcd::new(addressing_mode::VxIIndirect::new(register::GpRegisters::V0,)).generate(&cpu)
    );
}

#[test]
fn should_parse_read_registers_from_memory_operation() {
    let input: Vec<(usize, u8)> = inst_to_enumerated_be_byte_vec(0xF265u16);

    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: ReadRegistersFromMemory::new(addressing_mode::VxIIndirect::new(
                register::GpRegisters::V2,
            ))
        }),
        <ReadRegistersFromMemory<addressing_mode::VxIIndirect>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_read_registers_from_memory_operation() {
    use crate::address_map::Addressable;

    let base_i_addr = 0x200;
    let mut cpu = Chip8::<()>::default()
        .with_i_register(register::GeneralPurpose::<u16>::with_value(base_i_addr));

    for v in 1..4 {
        let addr_offset = base_i_addr + (v as u16 - 1);
        cpu.address_space.write(addr_offset, v).unwrap();
    }

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x01
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V1),
                0x02
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V2),
                0x03
            )),
        ],
        ReadRegistersFromMemory::new(addressing_mode::VxIIndirect::new(register::GpRegisters::V2))
            .generate(&cpu)
    );
}

#[test]
fn should_parse_store_registers_to_memory_operation() {
    let input: Vec<(usize, u8)> = inst_to_enumerated_be_byte_vec(0xF255u16);

    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: StoreRegistersToMemory::new(addressing_mode::VxIIndirect::new(
                register::GpRegisters::V2,
            ))
        }),
        <StoreRegistersToMemory<addressing_mode::VxIIndirect>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_store_registers_to_memory_operation() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0x01),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x02),
        )
        .with_gp_register(
            register::GpRegisters::V2,
            register::GeneralPurpose::<u8>::with_value(0x03),
        )
        .with_i_register(register::GeneralPurpose::<u16>::with_value(0x0100));

    assert_eq!(
        vec![
            Microcode::WriteMemory(WriteMemory::new(0x0100, 0x01)),
            Microcode::WriteMemory(WriteMemory::new(0x0101, 0x02)),
            Microcode::WriteMemory(WriteMemory::new(0x0102, 0x03)),
        ],
        StoreRegistersToMemory::new(addressing_mode::VxIIndirect::new(register::GpRegisters::V2))
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
    let cpu = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
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
fn should_generate_call_absolute_instruction() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_pc_register(register::ProgramCounter::with_value(0x200));

    assert_eq!(
        vec![
            // save initial value
            Microcode::PushStack(PushStack::new(0x200)),
            // jump to absolute value - 2.
            Microcode::Write16bitRegister(Write16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                0x3fe
            ))
        ],
        Call::new(addressing_mode::Absolute::new(u12::new(0x400))).generate(&cpu)
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
    let cpu = Chip8::<()>::default().with_rng(|| 0u8);
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
    let cpu = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
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
fn should_parse_add_vxvy_with_carry_operation() {
    let input: Vec<(usize, u8)> = 0x8014u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Add::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0))
        }),
        <Add<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_add_vxvy_with_carry_operation_that_overflows_when_sum_overflows_byte_capacity() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xfe),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x05),
        );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x03
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x01
            ))
        ],
        Add::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_generate_add_vxvy_with_carry_operation_that_does_not_set_overflow_when_under_byte() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xfe),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x01),
        );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0xff
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x00
            ))
        ],
        Add::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_parse_sub_vxvy_without_borrow_operation() {
    let input: Vec<(usize, u8)> = 0x8015u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Sub::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0))
        }),
        <Sub<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_set_borrow_for_sub_vxvy_if_vx_value_is_larger_than_vy_value() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x05),
        );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0xfa
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x01
            ))
        ],
        Sub::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_generate_not_set_borrow_for_sub_vxvy_if_vx_value_is_larger_than_vy_value() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0x01),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0xff),
        );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x02
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x00
            ))
        ],
        Sub::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_parse_subn_vxvy_without_borrow_operation() {
    let input: Vec<(usize, u8)> = 0x8017u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Subn::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0))
        }),
        <Subn<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_subn_vxvy_without_borrow_operation_that_doesnt_set_underflow_when_difference_underflows_byte_capacity(
) {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x05),
        );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x06
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x00
            ))
        ],
        Subn::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_generate_subn_vxvy_without_underflow_operation_that_does_set_underflow_when_difference_fits_within_byte(
) {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0x01),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0xff),
        );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0xfe
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x01
            ))
        ],
        Subn::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
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
            inner: And::new(addressing_mode::VxVy::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <And<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_and_byte_register_operation() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
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
        And::new(addressing_mode::VxVy::new(
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
            inner: Or::new(addressing_mode::VxVy::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Or<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_or_byte_register_operation() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
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
        Or::new(addressing_mode::VxVy::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_shl_vxvy_with_flag_operation() {
    let input: Vec<(usize, u8)> = 0x801Eu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Shl::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0))
        }),
        <Shl<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_shl_vxvy_with_flag_operation_that_overflows_byte_capacity() {
    let cpu = Chip8::<()>::default().with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0x81u8),
    );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x01
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x02
            ))
        ],
        Shl::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_generate_shl_vxvy_with_flag_operation_that_does_not_overflow_byte_capacity() {
    let cpu = Chip8::<()>::default().with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0x01),
    );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x0
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x2
            ))
        ],
        Shl::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_parse_shr_vxvy_with_flag_operation() {
    let input: Vec<(usize, u8)> = 0x8016u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Shr::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0))
        }),
        <Shr<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_shr_vxvy_with_flag_operation_that_overflows_byte_capacity() {
    let cpu = Chip8::<()>::default().with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0x03u8),
    );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x01
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x01
            ))
        ],
        Shr::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
    );
}

#[test]
fn should_generate_shr_vxvy_with_flag_operation_that_does_not_overflow_byte_capacity() {
    let cpu = Chip8::<()>::default().with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0x8),
    );

    assert_eq!(
        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                0x0
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::V0),
                0x4
            ))
        ],
        Shr::new(addressing_mode::VxVy::new(GpRegisters::V1, GpRegisters::V0)).generate(&cpu)
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
            inner: Xor::new(addressing_mode::VxVy::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Xor<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_xor_byte_register_operation() {
    let cpu = Chip8::<()>::default()
        .with_rng(|| 0u8)
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
        Xor::new(addressing_mode::VxVy::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu)
    );
}

#[test]
fn should_parse_se_immediate_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x30ffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Se::new(addressing_mode::Immediate::new(
                register::GpRegisters::V0,
                0xff
            ))
        }),
        <Se<addressing_mode::Immediate>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_se_immediate_operation() {
    let cpu_eq = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );

    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            2
        ))],
        Se::new(addressing_mode::Immediate::new(
            register::GpRegisters::V0,
            0xff
        ))
        .generate(&cpu_eq)
    );

    let cpu_ne = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );

    assert_eq!(
        Vec::<Microcode>::new(),
        Se::new(addressing_mode::Immediate::new(
            register::GpRegisters::V0,
            0x00
        ))
        .generate(&cpu_ne)
    );
}

#[test]
fn should_parse_se_byte_register_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x5010u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Se::new(addressing_mode::VxVy::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Se<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_se_byte_register_operation() {
    let cpu_eq = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        );
    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            2
        ))],
        Se::new(addressing_mode::VxVy::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu_eq)
    );

    let cpu_ne = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        );
    assert_eq!(
        Vec::<Microcode>::new(),
        Se::new(addressing_mode::VxVy::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu_ne)
    );
}

#[test]
fn should_parse_sne_immediate_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x40ffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Sne::new(addressing_mode::Immediate::new(
                register::GpRegisters::V0,
                0xff
            ))
        }),
        <Sne<addressing_mode::Immediate>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_sne_immediate_operation() {
    let cpu_eq = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );

    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            2
        ))],
        Sne::new(addressing_mode::Immediate::new(
            register::GpRegisters::V0,
            0x00
        ))
        .generate(&cpu_eq)
    );

    let cpu_ne = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0xff),
    );

    assert_eq!(
        Vec::<Microcode>::new(),
        Sne::new(addressing_mode::Immediate::new(
            register::GpRegisters::V0,
            0xff
        ))
        .generate(&cpu_ne)
    );
}

#[test]
fn should_parse_sne_byte_register_operation_opcode() {
    let input: Vec<(usize, u8)> = 0x9010u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Sne::new(addressing_mode::VxVy::new(
                register::GpRegisters::V1,
                register::GpRegisters::V0
            ))
        }),
        <Sne<addressing_mode::VxVy>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_sne_byte_register_operation() {
    let cpu_eq = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0x00),
        );
    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            2
        ))],
        Sne::new(addressing_mode::VxVy::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu_eq)
    );

    let cpu_ne = Chip8::<()>::default()
        .with_rng(|| 0u8)
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0xff),
        )
        .with_gp_register(
            register::GpRegisters::V1,
            register::GeneralPurpose::<u8>::with_value(0xff),
        );
    assert_eq!(
        Vec::<Microcode>::new(),
        Sne::new(addressing_mode::VxVy::new(
            register::GpRegisters::V1,
            register::GpRegisters::V0
        ))
        .generate(&cpu_ne)
    );
}

#[test]
fn should_parse_rnd_immediate_operation_opcode() {
    let input: Vec<(usize, u8)> = 0xc0ffu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Rnd::new(addressing_mode::Immediate::new(
                register::GpRegisters::V0,
                0xff
            ))
        }),
        <Rnd<addressing_mode::Immediate>>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_rnd_immediate_operation() {
    let cpu = Chip8::<()>::default().with_rng(|| 0u8).with_gp_register(
        register::GpRegisters::V0,
        register::GeneralPurpose::<u8>::with_value(0x00),
    );

    // run many times
    for _ in 0..256 {
        // Assert 0x00 mask returns 0x00
        assert_eq!(
            vec![Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(register::GpRegisters::V0),
                0
            ))],
            Rnd::new(addressing_mode::Immediate::new(
                register::GpRegisters::V0,
                0x00
            ))
            .generate(&cpu.clone())
        );

        // Assert 0x0f mask returns 0x00 - 0x0f
        let write_value = match Rnd::new(addressing_mode::Immediate::new(
            register::GpRegisters::V0,
            0x00,
        ))
        .generate(&cpu.clone())
        .get(0)
        {
            Some(Microcode::Write8bitRegister(op)) => op.value,
            _ => panic!("should container Write8BitRegister operation"),
        };

        assert!(write_value <= 0x0f);

        // Assert 0xf0 mask returns 0xf0 - 0xff
        let write_value = match Rnd::new(addressing_mode::Immediate::new(
            register::GpRegisters::V0,
            0xf0,
        ))
        .generate(&cpu.clone())
        .get(0)
        {
            Some(Microcode::Write8bitRegister(op)) => op.value,
            _ => panic!("should container Write8BitRegister operation"),
        };

        assert!(write_value <= 0xf0);
    }
}

#[test]
fn should_parse_skp_opcode() {
    let input: Vec<(usize, u8)> = 0xE09Eu16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Skp::new(register::GpRegisters::V0)
        }),
        <Skp>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_skp_operation() {
    // a cpu with an input value set that matches.
    let cpu_some_eq = Chip8::<()>::default()
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        )
        .with_input(|| Some(chip8::KeyInputValue::KeyF));

    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            2
        ))],
        Skp::new(register::GpRegisters::V0).generate(&cpu_some_eq)
    );

    // a cpu with an input value that doesn't match.
    let cpu_some_ne = cpu_some_eq
        .clone()
        .with_input(|| Some(chip8::KeyInputValue::Key0));

    assert_eq!(
        Vec::<Microcode>::new(),
        Skp::new(register::GpRegisters::V0).generate(&cpu_some_ne)
    );

    // a cpu without a key pressed.
    let cpu_none = cpu_some_eq.clone().with_input(|| None);

    assert_eq!(
        Vec::<Microcode>::new(),
        Skp::new(register::GpRegisters::V0).generate(&cpu_none)
    );
}

#[test]
fn should_parse_sknp_opcode() {
    let input: Vec<(usize, u8)> = 0xE0A1u16
        .to_be_bytes()
        .iter()
        .copied()
        .enumerate()
        .collect();
    assert_eq!(
        Ok(MatchStatus::Match {
            span: 0..2,
            remainder: &input[2..],
            inner: Sknp::new(register::GpRegisters::V0)
        }),
        <Sknp>::default().parse(&input[..])
    );
}

#[test]
fn should_generate_sknp_operation() {
    // a cpu with an input value set that matches.
    let cpu_some_eq = Chip8::<()>::default()
        .with_gp_register(
            register::GpRegisters::V0,
            register::GeneralPurpose::<u8>::with_value(0x0f),
        )
        .with_input(|| Some(chip8::KeyInputValue::KeyF));

    assert_eq!(
        Vec::<Microcode>::new(),
        Sknp::new(register::GpRegisters::V0).generate(&cpu_some_eq)
    );

    // a cpu with an input value that doesn't match.
    let cpu_some_ne = cpu_some_eq
        .clone()
        .with_input(|| Some(chip8::KeyInputValue::Key0));

    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            2
        ))],
        Sknp::new(register::GpRegisters::V0).generate(&cpu_some_ne)
    );

    // a cpu without a key pressed.
    let cpu_none = cpu_some_eq.clone().with_input(|| None);

    assert_eq!(
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            2
        ))],
        Sknp::new(register::GpRegisters::V0).generate(&cpu_none)
    );
}
