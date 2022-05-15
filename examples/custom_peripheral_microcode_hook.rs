//! A small demonstration example of mainspring showing a basic custom execution-time hook implementation.

use mainspring::address_map::memory::{Memory, ReadOnly, ReadWrite};
use mainspring::cpu::mos6502::microcode::Microcode;
use mainspring::cpu::mos6502::Mos6502;
use mainspring::cpu::Execute;

#[allow(unused)]
use mainspring::prelude::v1::*;

type Rom = Memory<ReadOnly, u16, u8>;
type Ram = Memory<ReadWrite, u16, u8>;

/// VIA functions as an analogy to the 6522 VIA chip. However, only a subset of
/// functionality has been implemented for this demo.
#[derive(Clone)]
pub struct Via {
    port_a_addr: u16, // address connected to enable port a CS
    port_b_addr: u16, // address connected to enable port a CS
    port_a: u8,
    port_b: u8,
}

impl Via {
    pub fn new(base_addr: u16) -> Self {
        Via {
            port_a_addr: base_addr,
            port_b_addr: base_addr + 1,
            port_a: 0,
            port_b: 0,
        }
    }
}

fn main() {
    // A small rom that loops over write 01010101 and 10101010 to port a where
    // it is output. this will loop endlessly until stopped.
    let rom = Rom::new(0xffea, 0xffff).load(vec![
        0xa9, 0xff, 0x8d, 0x02, 0x80, 0xa9, 0x55, 0x8d, 0x00, 0x80, 0xa9, 0xaa, 0x8d, 0x00, 0x80,
        0x4c, 0xef, 0xff, 0xea, 0xff, 0x00, 0x00,
    ]);

    let mut via = Via::new(0x8000);
    let cpu = Mos6502::default()
        // Registers the address space and the rom as addressable memory with
        // the cpu. This accepts any implementation of the Addressable trait.
        .register_address_space(0xffea..=0xffff, rom)
        // Registration can fail, this unwraps the result.
        .unwrap()
        // Register placeholder space for the via memory
        .register_address_space(0x8000..=0x8003, Ram::new(0x8000, 0x8003))
        .unwrap()
        // Resets the cpu and loads the reset vector into the PC.
        .reset()
        // This return a StepState<Mos6502> which is unwrapped to return the
        // enclosing cpu.
        .unwrap();

    // Runs the program for 80 cycles.
    let states: Vec<Microcode> = cpu
        .clone()
        .into_iter()
        .flat_map(Into::<Vec<Vec<Microcode>>>::into)
        .take(80)
        .flatten()
        .collect();

    // Microcode can then be folded onto a cpu to replay its state onto a fresh cpu.
    states
        .iter()
        .filter(|mc| match mc {
            Microcode::WriteMemory(address, value) if (0x8000..=0x8003).contains(address) => {
                if *address == via.port_a_addr {
                    via.port_a = *value;
                    println!("{:08b}", value);
                } else if *address == via.port_b_addr {
                    via.port_b = *value;
                }
                false
            }
            _ => true,
        })
        .fold(cpu, |c, mc| mc.execute(c));
}
