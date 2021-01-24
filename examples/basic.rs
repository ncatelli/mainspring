extern crate mainspring;
use mainspring::address_map::memory::{Memory, ReadOnly, ReadWrite};
use mainspring::cpu::mos6502::MOS6502;

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

    let state = cpu.run(1_000_000).unwrap();

    println!("{:?}", state);
}
