use crate::cpu::mos6502::{
    microcode::*,
    operations::{address_mode, mnemonic, Instruction, MOps, Operation},
    Generate, MOS6502,
};

macro_rules! inst_to_operation {
    ($mnemonic:expr, $addrmode:expr) => {
        Instruction::new($mnemonic, $addrmode).into()
    };
}

#[test]
fn should_generate_implied_address_mode_nop_machine_code() {
    let cpu = MOS6502::default();
    let op: Operation = inst_to_operation!(mnemonic::NOP, address_mode::Implied);
    let mc = op.generate(&cpu);

    // check Mops value is correct
    assert_eq!(MOps::new(1, 2, vec![]), mc);

    // validate mops -> vector looks correct
    assert_eq!(
        vec![vec![], vec![Microcode::IncPCRegister(IncPCRegister(1))]],
        Into::<Vec<Vec<Microcode>>>::into(mc)
    )
}
