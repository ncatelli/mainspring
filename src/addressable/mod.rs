use std::marker::PhantomData;

#[cfg(test)]
mod tests;

// Represents an error that happens in interactions with memory.
#[derive(Debug, Clone, Copy)]
pub enum MemoryErr {
    Load,
}

impl std::fmt::Display for MemoryErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Load => write!(f, "failed to load rom"),
        }
    }
}

/// Represents a ReadOnly type of memory. This is entirely used for
/// typechecking and has no other practical uses.
pub struct ReadOnly {}

/// Represents a ReadWrite type of memory. This is entirely used for
/// typechecking and has no other practical uses.
pub struct ReadWrite {}

/// Represents an addressable segment of memory, be it RAM or ROM.
#[allow(dead_code)]
pub struct Memory<T> {
    mem_type: PhantomData<T>,
    start_address: u16,
    stop_address: u16,
    buffer: [u8; std::u16::MAX as usize],
}

impl<T> Memory<T> {
    /// Allocates a new addressable memory module
    pub fn new(start_address: u16, stop_address: u16) -> Self {
        Memory {
            mem_type: PhantomData,
            start_address,
            stop_address,
            buffer: [0; std::u16::MAX as usize],
        }
    }

    /// Reads a single byte at the specified address
    pub fn read(&self, addr: u16) -> u8 {
        self.buffer[addr as usize]
    }

    /// Write assigns a single value to an address in memory
    pub fn write(&mut self, value: u8, addr: u16) -> u8 {
        self.buffer[addr as usize] = value;
        value
    }

    /// Dump memory to a Vector
    pub fn dump(&self) -> Vec<u8> {
        self.buffer.iter().copied().collect()
    }

    /// Load data into memory takes a rom and returns an instance of Memory
    /// with the newly loaded dataset.
    pub fn load(self, data: [u8; std::u16::MAX as usize]) -> Self {
        Memory {
            mem_type: self.mem_type,
            start_address: self.start_address,
            stop_address: self.stop_address,
            buffer: data,
        }
    }
}
