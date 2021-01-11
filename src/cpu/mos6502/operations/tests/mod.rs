extern crate parcel;
use crate::cpu::mos6502::operations::*;
use std::convert::TryFrom;

#[test]
fn should_parse_nop_instruction_from_bytecode() {
    let bytecode = [0xea, 0x00, 0x00];
    let operation = TryFrom::try_from(&bytecode);

    assert_eq!(
        Ok(Operation::new(mnemonic::NOP, address_mode::Implied)),
        operation
    );
}

#[test]
fn should_parse_immediate_address_mode_lda_instruction_from_bytecode() {
    let bytecode = [0xa9, 0x12, 0x34];
    let operation = TryFrom::try_from(&bytecode);

    assert_eq!(
        Ok(Operation::new(mnemonic::LDA, address_mode::Immediate(0x12))),
        operation
    );
}
