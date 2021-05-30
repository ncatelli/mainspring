use mainspring::address_map::memory::{Memory, ReadOnly, ReadWrite};
use mainspring::cpu::mos6502::Mos6502;

#[allow(unused)]
use mainspring::prelude::v1::*;

type Rom = Memory<ReadOnly, u16, u8>;
type Ram = Memory<ReadWrite, u16, u8>;

fn main() {
    // A ReadOnly memory segment containing a small rom consisting of a
    // LDA/STA loop. This will run until stopped. This exists in the address
    // space inclusively between 0xffea and 0xffff.
    let rom = Rom::new(0xffea, 0xffff).load(vec![
        0xa9, 0x01, 0x8d, 0x00, 0x02, 0xa9, 0x02, 0x8d, 0x01, 0x02, 0xa9, 0x03, 0x8d, 0x02, 0x02,
        0x4c, 0xea, 0xff, 0xea, 0xff, 0x00, 0x00,
    ]);

    // A segment of ReadWrite memory existing inclusively in the the space
    // between 0x8000 and 0xffff.
    let ram = Ram::new(0x0200, 0x7fff);
    let cpu = Mos6502::default()
        // Registers the address space and the rom as addressable memory with
        // the cpu. This accepts any implementation of the Addressable trait.
        .register_address_space(0xffea..=0xffff, rom)
        // Registration can fail, this unwraps the result.
        .unwrap()
        .register_address_space(0x0200..=0x7fff, ram)
        .unwrap()
        // Resets the cpu and loads the reset vector into the PC.
        .reset()
        // This return a StepState<Mos6502> which is unwrapped to return the
        // enclosing cpu.
        .unwrap();

    // Run the cpu for 1,000,000 cycles, and returns a StepState<Mos6502> which
    // is unwrapped to return a CPU snapshot at its current state.
    let state = cpu.run(1_000_000).unwrap();

    println!("{:#?}", state);
}
