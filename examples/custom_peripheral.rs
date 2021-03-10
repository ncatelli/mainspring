//! A small demonstration example of mainspring showing a basic custom Addressable implementation.

use mainspring::address_map::memory::{Memory, ReadOnly};
use mainspring::cpu::mos6502::microcode::Microcode;
use mainspring::cpu::mos6502::MOS6502;

#[allow(unused)]
use mainspring::prelude::v1::*;

/// VIA functions as an analogy to the 6522 VIA chip. However, only a subset of
/// functionality has been implemented for this demo. For the case of this example,
/// all ports are configured in write mode with predefined addresses.
#[derive(Clone)]
pub struct VIA {
    port_a_addr: u16, // address connected to enable port a CS
    port_b_addr: u16, // address connected to enable port a CS
    port_a_mask: u16,
    port_b_mask: u16,
    port_a: u8,
    port_b: u8,
}

impl VIA {
    pub fn new(base_addr: u16) -> Self {
        VIA {
            port_a_addr: base_addr,
            port_b_addr: base_addr + 1,
            port_a_mask: base_addr + 2,
            port_b_mask: base_addr + 3,
            port_a: 0,
            port_b: 0,
        }
    }
}

impl Addressable<u16> for VIA {
    // Read the value at an address. Since this device is never read a
    // constant is returned to satisfy the trait.
    fn read(&self, _: u16) -> u8 {
        0
    }

    /// Write a value to a a specified address. If the address matches port a,
    /// it will write the value to the console after storing it.
    fn write(&mut self, addr: u16, value: u8) -> Result<u8, String> {
        if addr == self.port_a_addr {
            self.port_a = value;
            println!("{:08b}", value);
            Ok(value)
        } else if addr == self.port_b_addr {
            self.port_b = value;
            Ok(value)
        } else if (self.port_a_mask..=self.port_b_mask).contains(&addr) {
            Ok(value)
        } else {
            Err(format!("unknown VIA write addrress: {}", addr))
        }
    }
}

fn main() {
    // A small rom that loops over write 01010101 and 10101010 to port a where
    // it is output. this will loop endlessly until stopped.
    let rom = Memory::<ReadOnly>::new(0xffea, 0xffff).load(vec![
        0xa9, 0xff, 0x8d, 0x02, 0x80, 0xa9, 0x55, 0x8d, 0x00, 0x80, 0xa9, 0xaa, 0x8d, 0x00, 0x80,
        0x4c, 0xef, 0xff, 0xea, 0xff, 0x00, 0x00,
    ]);

    let via = VIA::new(0x8000);
    let cpu = MOS6502::default()
        // Registers the address space and the rom as addressable memory with
        // the cpu. This accepts any implementation of the Addressable trait.
        .register_address_space(0xffea..=0xffff, rom)
        // Registration can fail, this unwraps the result.
        .unwrap()
        .register_address_space(0x8000..=0x8003, via)
        .unwrap()
        // Resets the cpu and loads the reset vector into the PC.
        .reset()
        // This return a StepState<MOS6502> which is unwrapped to return the
        // enclosing cpu.
        .unwrap();

    // Runs the program for 80 cycles.
    cpu.clone()
        .into_iter()
        .map(Into::<Vec<Vec<Microcode>>>::into)
        .flatten()
        .take(80)
        .flatten()
        .for_each(drop)
}
