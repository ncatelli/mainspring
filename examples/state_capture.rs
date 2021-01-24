extern crate mainspring;
use mainspring::address_map::memory::{Memory, ReadOnly, ReadWrite};
use mainspring::cpu::mos6502::{microcode::Microcode, Execute, MOS6502};

#[allow(unused)]
use mainspring::prelude::v1::*;

fn main() {
    let rom = Memory::<ReadOnly>::new(0x7fea, 0x7fff).load(vec![
        0xa9, 0x01, 0x8d, 0x00, 0x80, 0xa9, 0x02, 0x8d, 0x01, 0x80, 0xa9, 0x03, 0x8d, 0x02, 0x80,
        0x4c, 0xea, 0x7f, 0xea, 0x7f, 0x00, 0x00,
    ]);
    let ram = Memory::<ReadWrite>::new(0x8000, 0xffff);
    let cpu = MOS6502::default()
        .register_address_space(0x7fea..=0x7fff, rom)
        .unwrap()
        .register_address_space(0x8000..=0xffff, ram)
        .unwrap()
        .reset()
        .unwrap();

    let states: Vec<Microcode> = cpu
        .clone()
        .into_iter()
        .map(Into::<Vec<Vec<Microcode>>>::into)
        .flatten() // flatten instructions to cycles
        .take(1_000_000) // take 1,000,000 cycles
        .flatten()
        .collect();

    println!(
        "{:?}",
        states.into_iter().fold(cpu.clone(), |c, mc| mc.execute(c))
    );
}
