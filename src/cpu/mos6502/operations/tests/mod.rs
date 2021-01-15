extern crate parcel;
use std::convert::TryFrom;

macro_rules! gen_op_parse_assertion {
    ($bytecode:expr) => {
        assert!($crate::cpu::mos6502::operations::Operation::try_from($bytecode).is_ok())
    };
}

#[test]
fn should_parse_implied_address_mode_nop_instruction() {
    let bytecode = [0xea, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_immediate_address_mode_lda_instruction() {
    let bytecode = [0xa9, 0x12, 0x34];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_absolute_address_mode_sta_instruction() {
    let bytecode = [0x8d, 0x34, 0x12];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_absolute_address_mode_jmp_instruction() {
    let bytecode = [0x4c, 0x34, 0x12];
    gen_op_parse_assertion!(&bytecode);
}