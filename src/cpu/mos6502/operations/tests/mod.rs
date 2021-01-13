extern crate parcel;
use crate::cpu::mos6502::operations::*;
use std::convert::TryFrom;

#[test]
fn should_parse_implied_address_mode_nop_instruction() {
    let bytecode = [0xea, 0x00, 0x00];
    let operation = TryFrom::try_from(&bytecode);

    assert_eq!(
        Ok(Instruction::new(mnemonic::NOP, address_mode::Implied)),
        operation
    );
}

#[test]
fn should_parse_immediate_address_mode_lda_instruction() {
    let bytecode = [0xa9, 0x12, 0x34];
    let operation = TryFrom::try_from(&bytecode);

    assert_eq!(
        Ok(Instruction::new(
            mnemonic::LDA,
            address_mode::Immediate(0x12)
        )),
        operation
    );
}

#[test]
fn should_parse_absolute_address_mode_sta_instruction() {
    let bytecode = [0x8d, 0x34, 0x12];
    let operation = TryFrom::try_from(&bytecode);

    assert_eq!(
        Ok(Instruction::new(
            mnemonic::STA,
            address_mode::Absolute(0x1234)
        )),
        operation
    );
}

#[test]
fn should_parse_absolute_address_mode_jmp_instruction() {
    let bytecode = [0x4c, 0x34, 0x12];
    let operation = TryFrom::try_from(&bytecode);

    assert_eq!(
        Ok(Instruction::new(
            mnemonic::JMP,
            address_mode::Absolute(0x1234)
        )),
        operation
    );
}
