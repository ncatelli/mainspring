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

// BCC

#[test]
fn should_generate_bcc_machine_code_with_branch_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = false;

    let op: Operation = Instruction::new(mnemonic::BCC, address_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() + 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcc_machine_code_with_branch_and_page_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = false;

    let op: Operation = Instruction::new(mnemonic::BCC, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() - 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcc_machine_code_with_no_jump() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = true;

    let op: Operation = Instruction::new(mnemonic::BCC, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    assert_eq!(MOps::new(2, 2, vec![]), mc);
}

// BCS

#[test]
fn should_generate_bcs_machine_code_with_branch_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = true;

    let op: Operation = Instruction::new(mnemonic::BCS, address_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() + 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcs_machine_code_with_branch_and_page_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = true;

    let op: Operation = Instruction::new(mnemonic::BCS, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() - 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bcs_machine_code_with_no_jump() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.carry = false;

    let op: Operation = Instruction::new(mnemonic::BCS, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    assert_eq!(MOps::new(2, 2, vec![]), mc);
}

// BEQ

#[test]
fn should_generate_beq_machine_code_with_branch_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = true;

    let op: Operation = Instruction::new(mnemonic::BEQ, address_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() + 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_beq_machine_code_with_branch_and_page_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = true;

    let op: Operation = Instruction::new(mnemonic::BEQ, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() - 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_beq_machine_code_with_no_jump() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = false;

    let op: Operation = Instruction::new(mnemonic::BEQ, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    assert_eq!(MOps::new(2, 2, vec![]), mc);
}

// BNE

#[test]
fn should_generate_bne_machine_code_with_branch_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = false;

    let op: Operation = Instruction::new(mnemonic::BNE, address_mode::Relative(8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() + 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            3,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bne_machine_code_with_branch_and_page_penalty() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = false;

    let op: Operation = Instruction::new(mnemonic::BNE, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    // pc - relative address - inst size
    let pc = cpu.pc.read() - 8 - 2;

    assert_eq!(
        MOps::new(
            2,
            4,
            vec![gen_write_16bit_register_microcode!(WordRegisters::PC, pc)]
        ),
        mc
    );
}

#[test]
fn should_generate_bne_machine_code_with_no_jump() {
    let mut cpu = MOS6502::default().with_pc_register(ProgramCounter::with_value(0x6000));
    cpu.ps.zero = true;

    let op: Operation = Instruction::new(mnemonic::BNE, address_mode::Relative(-8)).into();
    let mc = op.generate(&cpu);

    assert_eq!(MOps::new(2, 2, vec![]), mc);
}

// CLC

#[test]
fn should_generate_implied_address_mode_clc_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.carry = true;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::CLC, address_mode::Implied).into();
    let mc = op.generate(&cpu);

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

    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Decimal, false),]
        ),
        mc
    );
}

// CLI

#[test]
fn should_generate_implied_address_mode_cli_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.interrupt_disable = true;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::CLI, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
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

// CLV

#[test]
fn should_generate_implied_address_mode_clv_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.overflow = true;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::CLV, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Overflow, false)]
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

// INC

#[test]
fn should_generate_implied_address_mode_inc_machine_code() {
    let mut cpu = MOS6502::default();
    cpu.address_map.write(0x01ff, 0x05).unwrap();
    let op: Operation = Instruction::new(mnemonic::INC, address_mode::Absolute(0x01ff)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
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

// INX

#[test]
fn should_generate_implied_address_mode_inx_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x12));
    let op: Operation = Instruction::new(mnemonic::INX, address_mode::Implied).into();
    let mc = op.generate(&cpu);

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

// JMP

#[test]
fn should_generate_indirect_address_mode_jmp_machine_code() {
    let mut cpu = MOS6502::default();
    let base_addr = 0x0100;
    let indirect_addr = 0x0150;
    cpu.address_map.write(base_addr, 0x50).unwrap();
    cpu.address_map.write(base_addr + 1, 0x01).unwrap();
    let op: Operation = Instruction::new(mnemonic::JMP, address_mode::Indirect(base_addr)).into();
    let mc = op.generate(&cpu);

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

// LDA

#[test]
fn should_generate_immediate_address_mode_lda_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::LDA, address_mode::Immediate(0xff)).into();
    let mc = op.generate(&cpu);

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

#[test]
fn should_generate_absolute_indexed_with_x_address_mode_lda_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x05));
    let op: Operation =
        Instruction::new(mnemonic::LDA, address_mode::AbsoluteIndexedWithX(0x0100)).into();
    let mc = op.generate(&cpu);

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

#[test]
fn should_generate_absolute_indexed_with_y_address_mode_lda_machine_code() {
    let cpu = MOS6502::default().with_gp_register(GPRegister::Y, GeneralPurpose::with_value(0x05));
    let op: Operation =
        Instruction::new(mnemonic::LDA, address_mode::AbsoluteIndexedWithY(0x0100)).into();
    let mc = op.generate(&cpu);

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

#[test]
fn should_generate_x_indexed_indirect_address_mode_lda_machine_code() {
    let mut cpu =
        MOS6502::default().with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap(); // indirect addr

    let op: Operation =
        Instruction::new(mnemonic::LDA, address_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            2,
            6,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xea)
            ]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xea),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_indirect_y_indexed_address_mode_lda_machine_code() {
    let mut cpu =
        MOS6502::default().with_gp_register(GPRegister::Y, GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap(); // indirect addr

    let op: Operation =
        Instruction::new(mnemonic::LDA, address_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            2,
            5,
            vec![
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xea)
            ]
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
                gen_flag_set_microcode!(ProgramStatusFlags::Negative, true),
                gen_flag_set_microcode!(ProgramStatusFlags::Zero, false),
                gen_write_8bit_register_microcode!(ByteRegisters::ACC, 0xea),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

// NOP

#[test]
fn should_generate_implied_address_mode_nop_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::NOP, address_mode::Implied).into();
    let mc = op.generate(&cpu);

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

// SEC

#[test]
fn should_generate_implied_address_mode_sec_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.carry = false;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::SEC, address_mode::Implied).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Carry, true),]
        ),
        mc
    );
}

// SED

#[test]
fn should_generate_implied_address_mode_sed_machine_code() {
    let mut ps = ProcessorStatus::new();
    ps.decimal = false;
    let cpu = MOS6502::default().with_ps_register(ps);
    let op: Operation = Instruction::new(mnemonic::SED, address_mode::Implied).into();
    let mc = op.generate(&cpu);

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

    assert_eq!(
        MOps::new(
            1,
            2,
            vec![gen_flag_set_microcode!(ProgramStatusFlags::Interrupt, true),]
        ),
        mc
    );
}

#[test]
fn should_generate_absolute_address_mode_jmp_machine_code() {
    let cpu = MOS6502::default();
    let addr = 0x0100;
    let op: Operation = Instruction::new(mnemonic::JMP, address_mode::Absolute(addr)).into();
    let mc = op.generate(&cpu);

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

// STA

#[test]
fn should_generate_absolute_address_mode_sta_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::STA, address_mode::Absolute(0x0100)).into();
    let mc = op.generate(&cpu);

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
fn should_generate_absolute_with_x_index_address_mode_sta_machine_code() {
    let cpu = MOS6502::default()
        .with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x05));
    let op: Operation =
        Instruction::new(mnemonic::STA, address_mode::AbsoluteIndexedWithX(0x0000)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
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
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_absolute_with_y_index_address_mode_sta_machine_code() {
    let cpu = MOS6502::default()
        .with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, GeneralPurpose::with_value(0x05));
    let op: Operation =
        Instruction::new(mnemonic::STA, address_mode::AbsoluteIndexedWithY(0x0000)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
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
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 3)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_x_indexed_indirect_address_mode_sta_machine_code() {
    let mut cpu = MOS6502::default()
        .with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();

    let op: Operation =
        Instruction::new(mnemonic::STA, address_mode::XIndexedIndirect(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            2,
            6,
            vec![Microcode::WriteMemory(WriteMemory::new(0xff, 0xff))]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![
                Microcode::WriteMemory(WriteMemory::new(0xff, 0xff)),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_indirect_y_indexed_address_mode_sta_machine_code() {
    let mut cpu = MOS6502::default()
        .with_gp_register(GPRegister::Y, GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();

    let op: Operation =
        Instruction::new(mnemonic::STA, address_mode::IndirectYIndexed(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            2,
            6,
            vec![Microcode::WriteMemory(WriteMemory::new(0xff, 0xff))]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![
                Microcode::WriteMemory(WriteMemory::new(0xff, 0xff)),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_zeropage_address_mode_sta_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = Instruction::new(mnemonic::STA, address_mode::ZeroPage(0x01)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            2,
            3,
            vec![Microcode::WriteMemory(WriteMemory::new(0x01, 0x00))]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![
                Microcode::WriteMemory(WriteMemory::new(0x01, 0x00)),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}

#[test]
fn should_generate_zeropage_with_x_index_address_mode_sta_machine_code() {
    let cpu = MOS6502::default()
        .with_gp_register(GPRegister::ACC, GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, GeneralPurpose::with_value(0x05));
    let op: Operation =
        Instruction::new(mnemonic::STA, address_mode::ZeroPageIndexedWithX(0x00)).into();
    let mc = op.generate(&cpu);

    assert_eq!(
        MOps::new(
            2,
            4,
            vec![Microcode::WriteMemory(WriteMemory::new(0x05, 0xff))]
        ),
        mc
    );

    assert_eq!(
        vec![
            vec![],
            vec![],
            vec![],
            vec![
                Microcode::WriteMemory(WriteMemory::new(0x05, 0xff)),
                gen_inc_16bit_register_microcode!(WordRegisters::PC, 2)
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
