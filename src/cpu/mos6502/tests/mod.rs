use crate::address_map::{
    memory::{Memory, ReadOnly, ReadWrite},
    Addressable,
};
use crate::cpu::{
    mos6502::{register, register::GPRegister, MOS6502},
    register::Register,
    CPU,
};

fn generate_test_cpu_with_instructions(opcodes: Vec<u8>) -> MOS6502 {
    let (start_addr, stop_addr) = (0x6000, 0x7000);
    let mut nop_sled = [0xea; 0x7000 - 0x6000].to_vec();
    for (index, val) in opcodes.into_iter().enumerate() {
        nop_sled[index] = val;
    }

    MOS6502::default()
        .reset()
        .unwrap()
        .with_pc_register(register::ProgramCounter::with_value(start_addr))
        .register_address_space(
            start_addr..stop_addr,
            Memory::<ReadOnly>::new(0x6000, 0x7000).load(nop_sled),
        )
        .unwrap()
}

#[test]
fn should_cycle_on_cmp_immediate_operation_with_inequal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc9, 0xff]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0x00),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert!(!state.ps.carry);
    assert!(!state.ps.negative);
    assert!(!state.ps.zero);
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_immediate_operation_with_equal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc9, 0xff]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0xff),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_lda_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa9, 0xff, 0xa9, 0x0f]);

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6004, state.pc.read());
    assert_eq!(0x0f, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_lda_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa5, 0xff]);

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x00, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_lda_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb5, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_lda_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0xad, 0x00, 0x02])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();

    // val in mem should be null
    assert_eq!(0x00, state.acc.read());
    assert_eq!(0x00, state.address_map.read(0x0200));
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_nop_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![]);
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6001, state.pc.read());

    // take 2 more cycles to validate ea has incremented again.
    let next_state = state.run(2).unwrap();
    assert_eq!(0x6002, next_state.pc.read());
}

#[test]
fn should_cycle_on_sta_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0x8d, 0x00, 0x02])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();

    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0x0200));
}

#[test]
fn should_cycle_on_jmp_absolute_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x4c, 0x50, 0x60]);

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6050, state.pc.read());
}

#[test]
fn should_cycle_on_jmp_indirect_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x6c, 0x50, 0x60]);
    let state = cpu.run(5).unwrap();

    assert_eq!(0xeaea, state.pc.read());
}

#[test]
fn should_cycle_on_txa_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x8a])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x00));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}
