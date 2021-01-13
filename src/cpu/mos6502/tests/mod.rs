use crate::address_map::memory::{Memory, ReadOnly};
use crate::cpu::{
    mos6502::{register, MOS6502},
    register::Register,
    StepState,
};

#[test]
fn should_cycle_on_nop_operation() {
    let nop_sled_start: u16 = 0x6000;
    let nop_sled_end: u16 = 0x7000;
    let mut nop_sled = Vec::<u8>::new();
    nop_sled.resize((nop_sled_end - nop_sled_start) as usize, 0xea);

    let mut cpu = MOS6502::default()
        .reset()
        .unwrap()
        .with_pc_register(register::ProgramCounter::with_value(0x6000));
    cpu.address_map = cpu
        .address_map
        .register(
            nop_sled_start..nop_sled_end,
            Box::new(Memory::<ReadOnly>::new(nop_sled_start, nop_sled_end).load(nop_sled)),
        )
        .unwrap();

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
fn should_cycle_on_jmp_operation() {
    let nop_sled_start: u16 = 0x6000;
    let nop_sled_end: u16 = 0x7000;
    let mut nop_sled = Vec::<u8>::new();
    nop_sled.resize((nop_sled_end - nop_sled_start) as usize, 0xea);
    nop_sled[1] = 0x4c; // jmp opcode
    nop_sled[2] = 0x00; // absolute address lsb
    nop_sled[3] = 0x60; // absolute address hsb

    let mut cpu = MOS6502::default()
        .reset()
        .unwrap()
        .with_pc_register(register::ProgramCounter::with_value(0x6000));
    cpu.address_map = cpu
        .address_map
        .register(
            nop_sled_start..nop_sled_end,
            Box::new(Memory::<ReadOnly>::new(nop_sled_start, nop_sled_end).load(nop_sled)),
        )
        .unwrap();

    // run 2 more cycles and validate next op, a jmp, is picked up
    let states: Vec<StepState<MOS6502>> = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .take(5)
        .collect();

    assert_eq!(1, states.first().unwrap().remaining);
    assert_eq!(0x6001, states.first().unwrap().cpu.pc.read());

    assert_eq!(0, states.last().unwrap().remaining);
    assert_eq!(0x6000, states.last().unwrap().cpu.pc.read());
}
