use crate::address_map::{
    memory::{Memory, ReadOnly, ReadWrite},
    Addressable,
};
use crate::cpu::{
    mos6502::{microcode, register, Execute, GPRegister, MicrocodeIntoIterator, MOS6502},
    register::Register,
    StepState,
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
fn should_cycle_on_nop_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![]);
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
    let state = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .nth(0)
        .unwrap();

    assert_eq!(1, state.remaining);
    assert_eq!(0x6002, state.cpu.pc.read());
    assert_eq!(0xff, state.cpu.acc.read());
}

#[test]
fn should_cycle_on_lda_immediate_mops() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa9, 0xff, 0xa9, 0x0f]);
    let state = MicrocodeIntoIterator { state: cpu.clone() }
        .into_iter()
        .take(2)
        .map(|mop| Into::<Vec<Vec<microcode::Microcode>>>::into(mop))
        .flatten()
        .flatten()
        .fold(cpu, |c, mc| mc.execute(c));

    assert_eq!(0x6004, state.pc.read());
    assert_eq!(0x0f, state.acc.read());
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

    let state = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .nth(3)
        .unwrap();

    assert_eq!(0, state.remaining);

    // val in mem should be null
    assert_eq!(0x00, state.cpu.acc.read());
    assert_eq!(0x00, state.cpu.address_map.read(0x0200));
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

    let state = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .nth(3)
        .unwrap();

    assert_eq!(0, state.remaining);
    assert_eq!(0xff, state.cpu.acc.read());
    assert_eq!(0xff, state.cpu.address_map.read(0x0200));
}

#[test]
fn should_cycle_on_jmp_absolute_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x4c, 0x50, 0x60]);
    let state = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .nth(2)
        .unwrap();

    assert_eq!(0, state.remaining);
    assert_eq!(0x6050, state.cpu.pc.read());
}

#[test]
fn should_cycle_on_jmp_indirect_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x6c, 0x50, 0x60]);
    let state = Into::<StepState<MOS6502>>::into(cpu)
        .into_iter()
        .nth(4)
        .unwrap();

    assert_eq!(0, state.remaining);
    assert_eq!(0xeaea, state.cpu.pc.read());
}
