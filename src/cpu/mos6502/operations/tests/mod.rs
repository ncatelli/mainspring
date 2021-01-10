extern crate parcel;
use crate::cpu::mos6502::operations::*;
use std::convert::TryFrom;

#[test]
fn should_parse_ea_instruction_from_bytecode() {
    let bytecode = [0xea, 0x00, 0x00];
    let operation = TryFrom::try_from(&bytecode);

    assert_eq!(
        Ok(Operation::new(mnemonic::NOP, address_mode::Implied)),
        operation
    );
}
