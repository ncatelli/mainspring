use crate::address_map::{
    memory::{Memory, ReadOnly},
    Addressable,
};
use crate::cpu::{
    mos6502::{
        microcode::*,
        operations::Operations,
        register::{
            ByteRegisters, GeneralPurpose, GpRegister, ProcessorStatus, ProgramCounter,
            ProgramStatusFlags, StackPointer, WordRegisters,
        },
        Generate, Mos6502,
    },
    register::Register,
};
use isa_mos6502::{addressing_mode, mnemonic, Instruction, InstructionVariant};

// Adc

#[test]
fn should_generate_absolute_addressing_mode_adc_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::Absolute(0x00ff)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_adc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_adc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::AbsoluteIndexedWithY(0x00fa)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_adc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xff).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_adc_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::Immediate(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_adc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xff).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_adc_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::ZeroPage(0xff)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_adc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x80))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0xff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Adc, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x7f)
            ]
        ),
        mc
    );
}

// And

#[test]
fn should_generate_absolute_addressing_mode_and_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_and_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x55))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_and_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x55))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::AbsoluteIndexedWithY(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_and_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_and_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::Immediate(0x55)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_and_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_and_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_and_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::And, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

// Asl

#[test]
fn should_generate_absolute_addressing_mode_asl_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Asl, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x54)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_asl_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Asl, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            7,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x54)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_accumulator_addressing_mode_asl_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xaa));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Asl, addressing_mode::Accumulator).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x54)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_asl_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Asl, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x54)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_asl_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Asl, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x54)
            ]
        ),
        mc
    );
}

// Bcc

#[test]
fn should_generate_bcc_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bcc, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcc_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bcc, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcc_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bcc, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Bcs

#[test]
fn should_generate_bcs_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bcs, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcs_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bcs, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcs_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bcs, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Beq

#[test]
fn should_generate_beq_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Beq, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_beq_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Beq, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_beq_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Beq, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Bmi

#[test]
fn should_generate_bmi_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.negative = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bmi, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc + relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bmi_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.negative = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bmi, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bmi_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.negative = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bmi, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Bit

#[test]
fn should_generate_absolute_addressing_mode_bit_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Bit, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_bit_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Bit, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
            ]
        ),
        mc
    );
}

// Bne

#[test]
fn should_generate_bne_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bne, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc + relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bne_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bne, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bne_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bne, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Bpl

#[test]
fn should_generate_bpl_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.negative = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bpl, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc + relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bpl_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.negative = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bpl, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bpl_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.negative = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bpl, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Brk

#[test]
fn should_generate_implied_addressing_mode_brk_machine_code() {
    let cpu = Mos6502::default()
        .reset()
        .unwrap()
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.brk = false;
            ps
        })
        .with_pc_register(ProgramCounter::with_value(0x1234))
        .register_address_space(
            0xfffe..=0xffff,
            Memory::<ReadOnly, u16, u8>::new(0xfffe, 0xffff).load(vec![0x78, 0x56]),
        )
        .unwrap();

    let op: InstructionVariant = Instruction::new(mnemonic::Brk, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    // expect unused flag to be set only for status register on stack.
    let expected_ps_on_stack = ProcessorStatus::with_value(0b00100000);

    assert_eq!(
        Operations::new(
            0, // PC controlled by the instruction
            7,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Break, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Interrupt, true),
                gen_write_memory_microcode!(0x01ff, 0x35), // PC (LL + 1)
                gen_dec_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_write_memory_microcode!(0x01fe, 0x12), // PC (HH)
                gen_dec_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_write_memory_microcode!(0x01fd, u8::from(expected_ps_on_stack)), // PS Register
                gen_dec_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_write_16bit_register_microcode!(WordRegisters::Pc, 0x5678),
            ]
        ),
        mc
    );
}

// Bvc

#[test]
fn should_generate_bvc_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.overflow = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bvc, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc + relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bvc_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.overflow = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bvc, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bvc_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.overflow = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bvc, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Bvs

#[test]
fn should_generate_bvs_machine_code_with_branch_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.overflow = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bvs, addressing_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc + relative address
    let pc = cpu.pc.read() + 8;

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bvs_machine_code_with_branch_and_page_penalty() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.overflow = true;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bvs, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address
    let pc = cpu.pc.read() - 8;

    assert_eq!(
        Operations::new(
            0,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bvs_machine_code_with_no_jump() {
    let mut cpu = Mos6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.overflow = false;

    let op: InstructionVariant =
        Instruction::new(mnemonic::Bvs, addressing_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    let pc = cpu.pc.read() + 2;

    assert_eq!(
        Operations::new(
            0,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, pc)]
        ),
        mc
    );
}

// Clc

#[test]
fn should_generate_implied_addressing_mode_clc_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.carry = true;
    let cpu = Mos6502::default().with_ps_register(ps);
    let op: InstructionVariant = Instruction::new(mnemonic::Clc, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),]
        ),
        mc
    );
}

// Cld

#[test]
fn should_generate_implied_addressing_mode_cld_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.carry = true;
    let cpu = Mos6502::default().with_ps_register(ps);
    let op: InstructionVariant = Instruction::new(mnemonic::Cld, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Decimal, false),]
        ),
        mc
    );
}

// Cli

#[test]
fn should_generate_implied_addressing_mode_cli_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.interrupt_disable = true;
    let cpu = Mos6502::default().with_ps_register(ps);
    let op: InstructionVariant = Instruction::new(mnemonic::Cli, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_flag_set_microcode!(
                ProgramStatusFlags::Interrupt,
                false
            )]
        ),
        mc
    );
}

// Clv

#[test]
fn should_generate_implied_addressing_mode_clv_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.overflow = true;
    let cpu = Mos6502::default().with_ps_register(ps);
    let op: InstructionVariant = Instruction::new(mnemonic::Clv, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Overflow, false)]
        ),
        mc
    );
}

// Cmp

#[test]
fn should_generate_absolute_addressing_mode_cmp_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cmp, addressing_mode::Absolute::default()).into();
    let mc = op.generate(&cpu);
    let expected_mops = vec![
        gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
        gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
        gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
    ];

    assert_eq!(Operations::new(3, 4, expected_mops.clone()), mc);
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_cmp_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant = Instruction::new(
        mnemonic::Cmp,
        addressing_mode::AbsoluteIndexedWithX::default(),
    )
    .into();
    let mc = op.generate(&cpu);
    let expected_mops = vec![
        gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
        gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
        gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
    ];

    assert_eq!(Operations::new(3, 4, expected_mops.clone()), mc);
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_cmp_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant = Instruction::new(
        mnemonic::Cmp,
        addressing_mode::AbsoluteIndexedWithY::default(),
    )
    .into();
    let mc = op.generate(&cpu);
    let expected_mops = vec![
        gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
        gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
        gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
    ];

    assert_eq!(Operations::new(3, 4, expected_mops.clone()), mc);
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_cmp_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05))
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xea));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Cmp, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_cmp_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cmp, addressing_mode::Immediate::default()).into();
    let mc = op.generate(&cpu);
    let expected_mops = vec![
        gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
        gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
        gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
    ];

    assert_eq!(Operations::new(2, 2, expected_mops.clone()), mc);
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_cmp_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xea));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Cmp, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_cmp_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cmp, addressing_mode::ZeroPage::default()).into();
    let mc = op.generate(&cpu);
    let expected_mops = vec![
        gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
        gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
        gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
    ];

    assert_eq!(Operations::new(2, 3, expected_mops.clone()), mc);
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_cmp_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant = Instruction::new(
        mnemonic::Cmp,
        addressing_mode::ZeroPageIndexedWithX::default(),
    )
    .into();
    let mc = op.generate(&cpu);
    let expected_mops = vec![
        gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
        gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
        gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
    ];

    assert_eq!(Operations::new(2, 4, expected_mops.clone()), mc);
}

// Cpx

#[test]
fn should_generate_absolute_addressing_mode_cpx_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cpx, addressing_mode::Absolute::default()).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_cpx_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cpx, addressing_mode::Immediate::default()).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_cpx_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cpx, addressing_mode::ZeroPage::default()).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

// Cpy

#[test]
fn should_generate_absolute_addressing_mode_cpy_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cpy, addressing_mode::Absolute::default()).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_cpy_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cpy, addressing_mode::Immediate::default()).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_cpy_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Cpy, addressing_mode::ZeroPage::default()).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
            ]
        ),
        mc
    );
}

// Dec

#[test]
fn should_generate_absolute_addressing_mode_dec_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0x01ff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Dec, addressing_mode::Absolute(0x01ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x01ff, 0x04),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_by_x_addressing_mode_dec_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x01ff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Dec, addressing_mode::AbsoluteIndexedWithX(0x01fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            7,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x01ff, 0x04),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_dec_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0xff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Dec, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0xff, 0x04),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_by_x_addressing_mode_dec_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0xff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Dec, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0xff, 0x04),
            ]
        ),
        mc
    );
}

// Dex

#[test]
fn should_generate_implied_addressing_mode_dex_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant = Instruction::new(mnemonic::Dex, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_dec_8bit_register_microcode!(ByteRegisters::X, 1),
            ]
        ),
        mc
    );
}

// Dey

#[test]
fn should_generate_implied_addressing_mode_dey_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant = Instruction::new(mnemonic::Dey, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_dec_8bit_register_microcode!(ByteRegisters::Y, 1),
            ]
        ),
        mc
    );
}

// Eor

#[test]
fn should_generate_absolute_addressing_mode_eor_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_eor_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x55))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_eor_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x55))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::AbsoluteIndexedWithY(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_eor_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_eor_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::Immediate(0x55)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_eor_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_eor_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_eor_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Eor, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xaa)
            ]
        ),
        mc
    );
}

// Inc

#[test]
fn should_generate_absolute_addressing_mode_inc_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0x01ff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Inc, addressing_mode::Absolute(0x01ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x01ff, 0x06),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_by_x_addressing_mode_inc_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x01ff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Inc, addressing_mode::AbsoluteIndexedWithX(0x01fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            7,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x01ff, 0x06),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_inc_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0xff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Inc, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0xff, 0x06),
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_by_x_addressing_mode_inc_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0xff, 0x05).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Inc, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0xff, 0x06),
            ]
        ),
        mc
    );
}

// Inx

#[test]
fn should_generate_implied_addressing_mode_inx_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x12));
    let op: InstructionVariant = Instruction::new(mnemonic::Inx, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::X, 0x13)
            ]
        ),
        mc
    );
}

// Iny

#[test]
fn should_generate_implied_addressing_mode_iny_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x12));
    let op: InstructionVariant = Instruction::new(mnemonic::Iny, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, 0x13)
            ]
        ),
        mc
    );
}

// Jmp

#[test]
fn should_generate_absolute_addressing_mode_jmp_machine_code() {
    let cpu = Mos6502::default();
    let addr = 0x0100;
    let op: InstructionVariant =
        Instruction::new(mnemonic::Jmp, addressing_mode::Absolute(addr)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            0,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::Pc, addr)]
        ),
        mc
    );
}

#[test]
fn should_generate_indirect_addressing_mode_jmp_machine_code() {
    let mut cpu = Mos6502::default();
    let base_addr = 0x0100;
    let indirect_addr = 0x0150;
    cpu.address_map.write(base_addr, 0x50).unwrap();
    cpu.address_map.write(base_addr + 1, 0x01).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Jmp, addressing_mode::Indirect(base_addr)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            0, // offset modified directly by operation
            5,
            vec![gen_write_16bit_register_microcode!(
                WordRegisters::Pc,
                indirect_addr
            )]
        ),
        mc
    );
}

// Jsr

#[test]
fn should_generate_absolute_addressing_mode_jsr_machine_code() {
    let cpu = Mos6502::default();
    let addr = 0x0100;
    let op: InstructionVariant =
        Instruction::new(mnemonic::Jsr, addressing_mode::Absolute(addr)).into();
    let mc = op.generate(&cpu);

    let sph: u16 = 0x0100 + cpu.sp.read() as u16;
    let spl: u16 = 0x0100 + (cpu.sp.read() - 1) as u16;
    let [pcl, pch] = cpu.pc.read().wrapping_add(2).to_le_bytes();

    assert_eq!(
        Operations::new(
            0, // offset modified directly by instruction
            6,
            vec![
                gen_write_memory_microcode!(sph, pch),
                gen_dec_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_write_memory_microcode!(spl, pcl),
                gen_dec_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_write_16bit_register_microcode!(WordRegisters::Pc, addr)
            ]
        ),
        mc
    );
}

// Lda

#[test]
fn should_generate_immediate_addressing_mode_lda_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::Immediate(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_lda_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::Acc,
                    0x00 // memory defaults to null
                )
            ]
        ),
        mc
    );

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x00),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_lda_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::ZeroPageIndexedWithX(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::Acc,
                    0xff // value at 0x05 in memory should be 0xff
                )
            ]
        ),
        mc
    );

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_absolute_addressing_mode_lda_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x00)
            ]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x00),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_lda_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::AbsoluteIndexedWithX(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x00)
            ]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x00),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_lda_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::AbsoluteIndexedWithY(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x00)
            ]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x00),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_lda_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xea)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_lda_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Lda, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xea)
            ]
        ),
        mc
    );
}

// Ldx

#[test]
fn should_generate_absolute_addressing_mode_ldx_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldx, addressing_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::X, 0x00)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_ldx_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldx, addressing_mode::AbsoluteIndexedWithY(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::X, 0x00)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_ldx_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldx, addressing_mode::Immediate(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::X, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_ldx_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldx, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::X,
                    0x00 // memory defaults to null
                )
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_y_addressing_mode_ldx_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldx, addressing_mode::ZeroPageIndexedWithY(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::X,
                    0xff // value at 0x05 in memory should be 0xff
                )
            ]
        ),
        mc
    );
}

// Ldy

#[test]
fn should_generate_absolute_addressing_mode_ldy_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldy, addressing_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, 0x00)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_ldy_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldy, addressing_mode::AbsoluteIndexedWithX(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, 0x00)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_ldy_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldy, addressing_mode::Immediate(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_ldy_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldy, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::Y,
                    0x00 // memory defaults to null
                )
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_ldy_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ldy, addressing_mode::ZeroPageIndexedWithX(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::Y,
                    0xff // value at 0x05 in memory should be 0xff
                )
            ]
        ),
        mc
    );
}

// Lsr

#[test]
fn should_generate_absolute_addressing_mode_lsr_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lsr, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x2a)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_lsr_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lsr, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            7,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x2a)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_accumulator_addressing_mode_lsr_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x55));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lsr, addressing_mode::Accumulator).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x2a)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_lsr_machine_code() {
    let mut cpu = Mos6502::default();
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lsr, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x2a)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_lsr_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Lsr, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x2a)
            ]
        ),
        mc
    );
}

// Nop

#[test]
fn should_generate_implied_addressing_mode_nop_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant = Instruction::new(mnemonic::Nop, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(Operations::new(1, 2, vec![]), mc);
}

// Ora

#[test]
fn should_generate_absolute_addressing_mode_ora_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_ora_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x55))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_ora_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x55))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::AbsoluteIndexedWithY(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_ora_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_ora_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::Immediate(0x55)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_ora_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_ora_machine_code() {
    let mut cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_ora_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ora, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

// Pha

#[test]
fn should_generate_implied_addressing_mode_pha_machine_code() {
    let cpu = Mos6502::default()
        .reset()
        .unwrap()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant = Instruction::new(mnemonic::Pha, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            3,
            vec![
                // should write to the top of the stack
                gen_write_memory_microcode!(0x01ff, 0xff),
                gen_dec_8bit_register_microcode!(ByteRegisters::Sp, 1),
            ]
        ),
        mc
    );
}

// Php

#[test]
fn should_generate_implied_addressing_mode_php_machine_code() {
    let cpu = Mos6502::default()
        .reset()
        .unwrap()
        .with_ps_register(ProcessorStatus::with_value(0x55));
    let op: InstructionVariant = Instruction::new(mnemonic::Php, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            3,
            vec![
                // should write to the top of the stack
                gen_write_memory_microcode!(0x01ff, 0x55),
                gen_dec_8bit_register_microcode!(ByteRegisters::Sp, 1),
            ]
        ),
        mc
    )
}

// Pla

#[test]
fn should_generate_implied_addressing_mode_pla_machine_code() {
    let mut cpu = Mos6502::default()
        .reset()
        .unwrap()
        // simulate having pushed teh value 0xff to the stack
        .with_sp_register(StackPointer::with_value(0xfe));
    cpu.address_map.write(0x01ff, 0xff).unwrap();

    let op: InstructionVariant = Instruction::new(mnemonic::Pla, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 1)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

// Plp

#[test]
fn should_generate_implied_addressing_mode_plp_machine_code() {
    let mut cpu = Mos6502::default()
        .reset()
        .unwrap()
        // simulate having pushed the value 0x55 from ps register to the stack
        .with_sp_register(StackPointer::with_value(0xfe));
    cpu.address_map.write(0x01ff, 0x55).unwrap();

    let op: InstructionVariant = Instruction::new(mnemonic::Plp, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_write_8bit_register_microcode!(ByteRegisters::Ps, 0x55),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 1)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

// Rol

#[test]
fn should_generate_absolute_addressing_mode_rol_machine_code() {
    let mut cpu = Mos6502::default().with_ps_register({
        let mut ps = ProcessorStatus::default();
        ps.carry = true;
        ps
    });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Rol, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_rol_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Rol, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            7,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_accumulator_addressing_mode_rol_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xaa))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    let op: InstructionVariant =
        Instruction::new(mnemonic::Rol, addressing_mode::Accumulator).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_rol_machine_code() {
    let mut cpu = Mos6502::default().with_ps_register({
        let mut ps = ProcessorStatus::default();
        ps.carry = true;
        ps
    });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Rol, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x55)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_rol_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Rol, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0x55)
            ]
        ),
        mc
    );
}

// Ror

#[test]
fn should_generate_absolute_addressing_mode_ror_machine_code() {
    let mut cpu = Mos6502::default().with_ps_register({
        let mut ps = ProcessorStatus::default();
        ps.carry = true;
        ps
    });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ror, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0xd5)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_ror_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ror, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            7,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0xd5)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_accumulator_addressing_mode_ror_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xaa))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ror, addressing_mode::Accumulator).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xd5)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_ror_machine_code() {
    let mut cpu = Mos6502::default().with_ps_register({
        let mut ps = ProcessorStatus::default();
        ps.carry = true;
        ps
    });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ror, addressing_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0xd5)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_ror_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Ror, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_memory_microcode!(0x00ff, 0xd5)
            ]
        ),
        mc
    );
}

// Rti

#[test]
fn should_generate_implied_addressing_mode_rti_machine_code() {
    let mut cpu = Mos6502::default().with_sp_register(StackPointer::with_value(0xfc));
    cpu.address_map.write(0x01ff, 0x12).unwrap();
    cpu.address_map.write(0x01fe, 0x34).unwrap();
    cpu.address_map
        .write(0x01fd, {
            let mut ps = ProcessorStatus::default();
            ps.interrupt_disable = false;
            ps.brk = false;
            u8::from(ps)
        })
        .unwrap();

    let op: InstructionVariant = Instruction::new(mnemonic::Rti, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            6,
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::Sp, 1),
                gen_write_8bit_register_microcode!(ByteRegisters::Ps, 0x20),
                gen_inc_8bit_register_microcode!(ByteRegisters::Sp, 2),
                gen_write_16bit_register_microcode!(WordRegisters::Pc, 0x1234)
            ]
        ),
        mc
    );
}

// Rts

#[test]
fn should_generate_implied_addressing_mode_rts_machine_code() {
    let mut cpu = Mos6502::default().with_sp_register(StackPointer::with_value(0xfd));
    cpu.address_map.write(0x01fe, 0x02).unwrap();
    cpu.address_map.write(0x01ff, 0x60).unwrap();

    let op: InstructionVariant = Instruction::new(mnemonic::Rts, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            6,
            vec![
                gen_inc_8bit_register_microcode!(ByteRegisters::Sp, 2),
                gen_write_16bit_register_microcode!(WordRegisters::Pc, 0x6002)
            ]
        ),
        mc
    );
}

// Sbc

#[test]
fn should_generate_absolute_addressing_mode_sbc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });

    cpu.address_map.write(0x00ff, 0xb0).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::Absolute(0x00ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_x_addressing_mode_sbc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::AbsoluteIndexedWithX(0x00fa)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_indexed_with_y_addressing_mode_sbc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::AbsoluteIndexedWithY(0x00fa)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_sbc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xb0).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_immediate_addressing_mode_sbc_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });

    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::Immediate(0xb0)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_sbc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xb0).unwrap(); // indirect addr

    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_sbc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::ZeroPage(0xff)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_indexed_with_x_addressing_mode_sbc_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0x50))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0xff, 0xb0).unwrap();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sbc, addressing_mode::ZeroPageIndexedWithX(0xfa)).into();

    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Overflow, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xa0)
            ]
        ),
        mc
    );
}

// Sec

#[test]
fn should_generate_implied_addressing_mode_sec_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.carry = false;
    let cpu = Mos6502::default().with_ps_register(ps);
    let op: InstructionVariant = Instruction::new(mnemonic::Sec, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),]
        ),
        mc
    );
}

// Sed

#[test]
fn should_generate_implied_addressing_mode_sed_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.decimal = false;
    let cpu = Mos6502::default().with_ps_register(ps);
    let op: InstructionVariant = Instruction::new(mnemonic::Sed, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Decimal, true),]
        ),
        mc
    );
}

// Sei

#[test]
fn should_generate_implied_addressing_mode_sei_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.interrupt_disable = false;
    let cpu = Mos6502::default().with_ps_register(ps);
    let op: InstructionVariant = Instruction::new(mnemonic::Sei, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Interrupt, true),]
        ),
        mc
    );
}

// Sta

#[test]
fn should_generate_absolute_addressing_mode_sta_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sta, addressing_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x0100, 0x00))]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_with_x_index_addressing_mode_sta_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sta, addressing_mode::AbsoluteIndexedWithX(0x0000)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            5,
            vec![Microcode::WriteMemory(WriteMemory::new(0x05, 0xff))]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![],
            vec![
                Microcode::WriteMemory(WriteMemory::new(0x05, 0xff)),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_absolute_with_y_index_addressing_mode_sta_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sta, addressing_mode::AbsoluteIndexedWithY(0x0000)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            5,
            vec![Microcode::WriteMemory(WriteMemory::new(0x05, 0xff))]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![],
            vec![
                Microcode::WriteMemory(WriteMemory::new(0x05, 0xff)),
                gen_inc_16bit_register_microcode!(WordRegisters::Pc, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_indirect_y_indexed_addressing_mode_sta_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05))
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();

    let op: InstructionVariant =
        Instruction::new(mnemonic::Sta, addressing_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![Microcode::WriteMemory(WriteMemory::new(0xff, 0xff))]
        ),
        mc
    );
}

#[test]
fn should_generate_x_indexed_indirect_addressing_mode_sta_machine_code() {
    let mut cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();

    let op: InstructionVariant =
        Instruction::new(mnemonic::Sta, addressing_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            6,
            vec![Microcode::WriteMemory(WriteMemory::new(0xff, 0xff))]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_sta_machine_code() {
    let cpu = Mos6502::default();
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sta, addressing_mode::ZeroPage(0x01)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![Microcode::WriteMemory(WriteMemory::new(0x01, 0x00))]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_with_x_index_addressing_mode_sta_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff))
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sta, addressing_mode::ZeroPageIndexedWithX(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x05, 0xff))]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_addressing_mode_stx_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x55));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Stx, addressing_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x0100, 0x55))]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_stx_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x55));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Stx, addressing_mode::ZeroPage(0x01)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![Microcode::WriteMemory(WriteMemory::new(0x01, 0x55))]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_with_y_index_addressing_mode_stx_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x55))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x05));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Stx, addressing_mode::ZeroPageIndexedWithY(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x05, 0x55))]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_addressing_mode_sty_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x55));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sty, addressing_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            3,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x0100, 0x55))]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_addressing_mode_sty_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x55));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sty, addressing_mode::ZeroPage(0x01)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            3,
            vec![Microcode::WriteMemory(WriteMemory::new(0x01, 0x55))]
        ),
        mc
    );
}

#[test]
fn should_generate_zeropage_with_x_index_addressing_mode_sty_machine_code() {
    let cpu = Mos6502::default()
        .with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x05))
        .with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0x55));
    let op: InstructionVariant =
        Instruction::new(mnemonic::Sty, addressing_mode::ZeroPageIndexedWithX(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            2,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x05, 0x55))]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_addressing_mode_tax_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant = Instruction::new(mnemonic::Tax, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::X, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_addressing_mode_tay_machine_code() {
    let cpu =
        Mos6502::default().with_gp_register(GpRegister::Acc, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant = Instruction::new(mnemonic::Tay, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Y, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_addressing_mode_tsx_machine_code() {
    let cpu = Mos6502::default().with_sp_register(StackPointer::default());
    let op: InstructionVariant = Instruction::new(mnemonic::Tsx, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::X, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_addressing_mode_txa_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant = Instruction::new(mnemonic::Txa, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_addressing_mode_txs_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::X, GeneralPurpose::with_value(0x00));
    let op: InstructionVariant = Instruction::new(mnemonic::Txs, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![gen_write_8bit_register_microcode!(ByteRegisters::Sp, 0x00)]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_addressing_mode_tya_machine_code() {
    let cpu = Mos6502::default().with_gp_register(GpRegister::Y, GeneralPurpose::with_value(0xff));
    let op: InstructionVariant = Instruction::new(mnemonic::Tya, addressing_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        Operations::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::Acc, 0xff)
            ]
        ),
        mc
    );
}
