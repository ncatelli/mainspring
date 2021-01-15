use crate::address_map::{
    memory::{Memory, ReadOnly, ReadWrite},
    Addressable,
};
use crate::cpu::{
    mos6502::{register, GPRegister, MOS6502},
    register::Register,
    StepState,
};

fn generate_test_cpu_with_instructions(opcodes: Vec<u8>) -> MOS6502 {
    let (start_addr, stop_addr) = (0x6000, 0x7000);
    let mut nop_sled = [0xea; 0x7000 - 0x6000].to_vec();
    for (index, val) in opcodes.into_iter().enumerate() {
        nop_sled[index] = val;
    }

    let cpu = MOS6502::default()
        .reset()
        .unwrap()
        .with_pc_register(register::ProgramCounter::with_value(start_addr))
        .register_address_space(
            start_addr..stop_addr,
            Memory::<ReadOnly>::new(0x6000, 0x7000).load(nop_sled),
        )
        .unwrap();
    cpu
}

#[test]
fn should_cycle_on_nop_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![]);

    // validate first step execs
    let states: Vec<StepState<MOS6502>> = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .take(3)
        .collect();
    assert_eq!(1, states.first().unwrap().remaining);
    assert_eq!(0x6001, states.first().unwrap().cpu.pc.read());

    // run 2 more cycles and validate next nop is picked up
    assert_eq!(1, states.last().unwrap().remaining);
    assert_eq!(0x6002, states.last().unwrap().cpu.pc.read());
}

#[test]
fn should_cycle_on_lda_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa9, 0xff]);

    let states: Vec<StepState<MOS6502>> = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .take(2)
        .collect();

    assert_eq!(1, states.first().unwrap().remaining);
    assert_eq!(0x6002, states.first().unwrap().cpu.pc.read());
    assert_eq!(0xff, states.first().unwrap().cpu.acc.read());
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

    let states: Vec<StepState<MOS6502>> = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .take(4)
        .collect();

    assert_eq!(0, states.last().unwrap().remaining);
    assert_eq!(0xff, states.last().unwrap().cpu.acc.read());
    assert_eq!(0xff, states.last().unwrap().cpu.address_map.read(0x0200));
}

#[test]
fn should_cycle_on_jmp_absolute_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xea, 0x4c, 0x50, 0x60]);

    let states: Vec<StepState<MOS6502>> = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .take(5)
        .collect();

    assert_eq!(1, states.first().unwrap().remaining);
    assert_eq!(0x6001, states.first().unwrap().cpu.pc.read());

    assert_eq!(0, states.last().unwrap().remaining);
    assert_eq!(0x6050, states.last().unwrap().cpu.pc.read());
}
