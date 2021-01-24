extern crate parcel;
use std::convert::TryFrom;

#[cfg(test)]
mod code_generation;

macro_rules! gen_op_parse_assertion {
    ($bytecode:expr) => {
        assert!($crate::cpu::mos6502::operations::Operation::try_from($bytecode).is_ok())
    };
}

#[test]
fn should_parse_relative_address_mode_beq_instruction() {
    let bytecode = [0xf0, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_relative_address_mode_bne_instruction() {
    let bytecode = [0xd0, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_clc_instruction() {
    let bytecode = [0x18, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_cld_instruction() {
    let bytecode = [0xd8, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_inc_instruction() {
    let bytecode = [0xee, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_inx_instruction() {
    let bytecode = [0xe8, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_iny_instruction() {
    let bytecode = [0xc8, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
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
fn should_parse_zeropage_address_mode_lda_instruction() {
    let bytecode = [0xa5, 0x12, 0x34];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_zeropage_with_x_index_address_mode_lda_instruction() {
    let bytecode = [0xb5, 0x12, 0x34];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_absolute_address_mode_jmp_instruction() {
    let bytecode = [0x4c, 0x34, 0x12];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_absolute_address_mode_sed_instruction() {
    let bytecode = [0xf8, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_absolute_address_mode_sei_instruction() {
    let bytecode = [0x78, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_absolute_address_mode_sta_instruction() {
    let bytecode = [0x8d, 0x34, 0x12];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_tax_instruction() {
    let bytecode = [0xaa, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_tay_instruction() {
    let bytecode = [0xa8, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_tsx_instruction() {
    let bytecode = [0xba, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_txa_instruction() {
    let bytecode = [0x8a, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_txs_instruction() {
    let bytecode = [0x9a, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}

#[test]
fn should_parse_implied_address_mode_tya_instruction() {
    let bytecode = [0x98, 0x00, 0x00];
    gen_op_parse_assertion!(&bytecode);
}
