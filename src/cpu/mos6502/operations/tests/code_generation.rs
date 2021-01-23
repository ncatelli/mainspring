use crate::address_map::Addressable;
use crate::cpu::mos6502::{
    microcode::*,
    operations::{address_mode, mnemonic, Instruction, MOps, Operation},
    register::{
        ByteRegisters, GPRegister, GeneralPurpose, ProcessorStatus, ProgramCounter,
        ProgramStatusFlags, StackPointer, WordRegisters,
    },
    Generate, MOS6502,
};
use crate::cpu::register::Register;

// BEQ

#[test]
fn should_generate_relative_address_mode_beq_machine_code() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));

    // case should jump
    cpu.ps.zero = true;

    let op: Operation = Instruction::new(mnemonic::BEQ, address_mode::Relative(-8)).into();
    let branch_case_true_mc = op.generate(&cpu);

    // case shouldn't ujump
    cpu.ps.zero = false;

    let op: Operation = Instruction::new(mnemonic::BEQ, address_mode::Relative(-8)).into();
    let branch_case_false_mc = op.generate(&cpu);

    let pc = cpu.pc.read() - 8 - 2; // pc - relative address - inst size

    assert_eq!(
        MOps::new(
            2,
            2,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        branch_case_true_mc
    );

    // check Mops value is correct
    assert_eq!(MOps::new(2, 2, vec![]), branch_case_false_mc);
}

// CLC

#[test]
fn should_generate_implied_address_mode_clc_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.carry = true;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::CLC, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Carry, false),]
        ),
        mc
    );
}

// CLD

#[test]
fn should_generate_implied_address_mode_cld_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.carry = true;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::CLD, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Decimal, false),]
        ),
        mc
    );
}

// CMP

#[test]
fn should_generate_immediate_address_mode_cmp_machine_code() {
    let cpu =
        MOS6502::default().with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0x00));
    let op: Operation = Instruction::new(mnemonic::CMP, address_mode::Immediate::default()).into();
    let mc = op.generate(&cpu);
    let expected_mops = vec![
        gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),
        gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
        gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
    ];

    // check Mops value is correct
    assert_eq!(MOps::new(2, 2, expected_mops.clone()), mc);

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            expected_mops
                .clone()
                .into_iter()
                .chain(vec![gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)].into_iter())
                .collect()
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

// INX

#[test]
fn should_generate_implied_address_mode_inx_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x12));
    let op: Operation = Instruction::new(mnemonic::INX, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
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

// INY

#[test]
fn should_generate_implied_address_mode_iny_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::Y, GeneralPurpose::with_value(0x12));
    let op: Operation = Instruction::new(mnemonic::INY, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
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

// NOP

#[test]
fn should_generate_implied_address_mode_nop_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::NOP, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(MOps::new(1, 2, vec![]), mc);

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            vec![gen_inc_16bit_register_microcode!(WordRegisters::PC, 1)]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

// LDA

#[test]
fn should_generate_immediate_address_mode_lda_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::LDA, address_mode::Immediate(0xff)).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            2,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xff)
            ]
        ),
        mc
    );

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xff),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_zeropage_address_mode_lda_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::LDA, address_mode::ZeroPage(0xff)).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            2,
            3,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::ACC,
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
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0x00),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_zeropage_indexed_with_x_address_mode_lda_machine_code() {
    let mut cpu =
        MOS6502::default().with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    let op: Operation =
        Instruction::new(mnemonic::LDA, address_mode::ZeroPageIndexedWithX(0x00)).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            2,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(
                    ByteRegisters::ACC,
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
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xff),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_absolute_address_mode_lda_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::LDA, address_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            3,
            4,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0x00)
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
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, false),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, true),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0x00),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

// SED

#[test]
fn should_generate_implied_address_mode_sed_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.decimal = false;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::SED, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Decimal, true),]
        ),
        mc
    );
}

// SEI

#[test]
fn should_generate_implied_address_mode_sei_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.interrupt_disable = false;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::SEI, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Interrupt, true),]
        ),
        mc
    );
}

// STA

#[test]
fn should_generate_absolute_address_mode_sta_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::STA, address_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            3,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x0100, 0x00))]
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
                Microcode::WriteMemory(WriteMemory::new(0x0100, 0x00)),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_absolute_address_mode_jmp_machine_code() {
    let cpu = MOS6502::default();
    let addr = 0x0100;
    let op: Operation = Instruction::new(mnemonic::JMP, address_mode::Absolute(addr)).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            3,
            3,
            vec![Microcode::Write16bitRegister(Write16bitRegister::new(
                WordRegisters::PC,
                addr - 3
            ))]
        ),
        mc
    );

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![
                Microcode::Write16bitRegister(Write16bitRegister::new(WordRegisters::PC, addr - 3)),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_indirect_address_mode_jmp_machine_code() {
    let mut cpu = MOS6502::default();
    let base_addr = 0x0100;
    let indirect_addr = 0x0150;
    cpu.address_map.write(base_addr, 0x50).unwrap();
    cpu.address_map.write(base_addr + 1, 0x01).unwrap();
    let op: Operation = Instruction::new(mnemonic::JMP, address_mode::Indirect(base_addr)).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            3,
            5,
            vec![Microcode::Write16bitRegister(Write16bitRegister::new(
                WordRegisters::PC,
                indirect_addr - 3
            )),]
        ),
        mc
    );

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![],
            vec![
                Microcode::Write16bitRegister(Write16bitRegister::new(
                    WordRegisters::PC,
                    indirect_addr - 3
                )),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_implied_address_mode_tax_machine_code() {
    let cpu =
        MOS6502::default().with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0xff));
    let op: Operation = Instruction::new(mnemonic::TAX, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
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
fn should_generate_implied_address_mode_tay_machine_code() {
    let cpu =
        MOS6502::default().with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0xff));
    let op: Operation = Instruction::new(mnemonic::TAY, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
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
fn should_generate_implied_address_mode_tsx_machine_code() {
    let cpu = MOS6502::default().with_sp_register(StackPointer::default());
    let op: Operation = Instruction::new(mnemonic::TSX, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
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
fn should_generate_implied_address_mode_txa_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::X, GeneralPurpose::with_value(0xff));
    let op: Operation = Instruction::new(mnemonic::TXA, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xff)
            ]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_address_mode_txs_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x00));
    let op: Operation = Instruction::new(mnemonic::TXS, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_write_8bit_register_microcode!(ByteRegisters::SP, 0x00)]
        ),
        mc
    );
}

#[test]
fn should_generate_implied_address_mode_tya_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::Y, GeneralPurpose::with_value(0xff));
    let op: Operation = Instruction::new(mnemonic::TYA, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(
        MOps::new(
            1,
            2,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xff)
            ]
        ),
        mc
    );
}
