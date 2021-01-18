use crate::address_map::Addressable;
use crate::cpu::mos6502::{
    microcode::*,
    operations::{address_mode, mnemonic, Instruction, MOps, Operation},
    register::{ByteRegisters, GPRegister, GeneralPurpose, WordRegisters},
    Generate, MOS6502,
};
use crate::cpu::register::Register;

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
            vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
                WordRegisters::PC,
                1
            ))]
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
            vec![Microcode::Write8bitRegister(Write8bitRegister::new(
                ByteRegisters::ACC,
                0xff
            ))]
        ),
        mc
    );

    // validate mops -> vector looks correct
    assert_eq!(
        vec![
            vec![],
            vec![
                Microcode::Write8bitRegister(Write8bitRegister::new(ByteRegisters::ACC, 0xff)),
                Microcode::Inc16bitRegister(Inc16bitRegister::new(WordRegisters::PC, 2))
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
            vec![Microcode::Write8bitRegister(Write8bitRegister::new(
                ByteRegisters::ACC,
                0x00 // memory defaults to null
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
                Microcode::Write8bitRegister(Write8bitRegister::new(ByteRegisters::ACC, 0x00)),
                Microcode::Inc16bitRegister(Inc16bitRegister::new(WordRegisters::PC, 2))
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
            vec![Microcode::Write8bitRegister(Write8bitRegister::new(
                ByteRegisters::ACC,
                0xff // value at 0x05 in memory should be 0xff
            ))]
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
                Microcode::Write8bitRegister(Write8bitRegister::new(ByteRegisters::ACC, 0xff)),
                Microcode::Inc16bitRegister(Inc16bitRegister::new(WordRegisters::PC, 2))
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
            vec![Microcode::Write8bitRegister(Write8bitRegister::new(
                ByteRegisters::ACC,
                0x00
            ))]
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
                Microcode::Write8bitRegister(Write8bitRegister::new(ByteRegisters::ACC, 0x00)),
                Microcode::Inc16bitRegister(Inc16bitRegister::new(WordRegisters::PC, 3))
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
                Microcode::Inc16bitRegister(Inc16bitRegister::new(WordRegisters::PC, 3))
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
                Microcode::Inc16bitRegister(Inc16bitRegister::new(WordRegisters::PC, 3))
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
                Microcode::Inc16bitRegister(Inc16bitRegister::new(WordRegisters::PC, 3))
            ]
        ],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}
