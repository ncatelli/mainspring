extern crate mainspring;
use mainspring::address_map::memory::{Memory, ReadOnly, ReadWrite};
use mainspring::cpu::mos6502::MOS6502;

#[allow(unused)]
use mainspring::prelude::v1::*;

fn main() {
    // A ReadOnly memory segment containing a small rom consisting of a
    // LDA/STA loop. This will run until stopped. This exists in the address
    // space inclusively beteen 0x7fea and 0x7fff.
    let rom = Memory::<ReadOnly>::new(0xffea, 0xffff).load(vec![
        0xa9, 0x01, 0x8d, 0x00, 0x02, 0xa9, 0x02, 0x8d, 0x01, 0x02, 0xa9, 0x03, 0x8d, 0x02, 0x02,
        0x4c, 0xea, 0xff, 0xea, 0xff, 0x00, 0x00,
    ]);

    // A segment of ReadWrite memory existing inclusively in the the space
    // between 0x8000 and 0xffff.
    let ram = Memory::<ReadWrite>::new(0x0200, 0x7fff);
    let cpu = MOS6502::default()
        // Registers the address space and the rom as addressable memory with
        // the cpu. This accepts any implementation of the Addressable trait.
        .register_address_space(0xffea..=0xffff, rom)
        // Registration can fail, this unwraps the result.
        .unwrap()
        .register_address_space(0x0200..=0x7fff, ram)
        .unwrap()
        // Resets the cpu and loads the reset vector into the PC.
        .reset()
        // This return a StepState<MOS6502> which is unwrapped to return the
        // enclosing cpu.
        .unwrap();

    // Run the cpu for 1,000,000 cycles, and returns a StepState<MOS6502> which
    // we unwrap to return a CPU snapshotted at it's state.
    let state = cpu.run(1_000_000).unwrap();

    println!("{:?}", state);
}
