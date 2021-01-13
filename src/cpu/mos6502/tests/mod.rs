use crate::address_map::memory::{Memory, ReadOnly};
use crate::cpu::{
    mos6502::{register, MOS6502},
    register::Register,
    CPU,
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
    let state = cpu.step();
    assert_eq!(1, state.remaining);
    assert_eq!(0x6001, state.cpu.pc.read());

    // run 2 more cycles and validate next nop is picked up
    let state = state.step().step();
    assert_eq!(1, state.remaining);
    assert_eq!(0x6002, state.cpu.pc.read());
}
