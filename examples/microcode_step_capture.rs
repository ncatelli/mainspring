use mainspring::address_map::memory::{Memory, ReadOnly, ReadWrite};
use mainspring::cpu::mos6502::{microcode::Microcode, Execute, MOS6502};

#[allow(unused)]
use mainspring::prelude::v1::*;

fn main() {
    // A ReadOnly memory segment containing a small rom consisting of a
    // LDA/STA loop. This will run until stopped. This exists in the address
    // space inclusively beteen 0xffea and 0xffff.
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

    // States represents a flattened vector of Microcode, or small cpu
    // operations that are generated by each instruction as they are
    // interpreted. These instructions are small enough to make storage viable
    // and can be replayed to any position in time of the collected
    // instructions.
    let states: Vec<Microcode> = cpu
        .clone()
        .into_iter()
        // converts the cpu into an emitter of instructions. Each sub-vector
        // represents the cycles and microcode generated by instruction that is
        // queued to execute next.
        .map(Into::<Vec<Vec<Microcode>>>::into)
        .flatten() // Flatten instructions to cycles
        .take(1_000_000) // Take 1,000,000 cycles.
        .flatten() // Flatten the cycles to a vector of microcode.
        .collect();

    println!(
        "{:?}",
        // Microcode can then be folded onto a cpu to replay it's state onto a fresh cpu.
        states.into_iter().fold(cpu.clone(), |c, mc| mc.execute(c))
    );
}
