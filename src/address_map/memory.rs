use crate::address_map::Addressable;
use std::marker::PhantomData;

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
    inner: Vec<u8>,
}

impl<T> Memory<T> {
    /// Allocates a new addressable memory module taking both a start and stop
    /// address.
    pub fn new(start_address: u16, stop_address: u16) -> Self {
        let mut data = Vec::new();
        data.resize((stop_address - start_address) as usize, 0);
        Memory {
            mem_type: PhantomData,
            start_address,
            stop_address,
            inner: data,
        }
    }

    /// Dump converts the current state of memroy into a correspnding Vec<u8>.
    pub fn dump(&self) -> Vec<u8> {
        self.inner.clone()
    }

    /// Load data into memory takes a rom and returns an instance of Memory
    /// with the newly loaded dataset.
    pub fn load(self, data: Vec<u8>) -> Self {
        Memory {
            mem_type: self.mem_type,
            start_address: self.start_address,
            stop_address: self.stop_address,
            inner: data,
        }
    }
}

impl Addressable<u16> for Memory<ReadWrite> {
    /// Reads a single byte at the specified address returning the u8
    /// representation of the value.
    fn read(&self, addr: u16) -> u8 {
        let addr_offset = addr - self.start_address;
        self.inner[usize::from(addr_offset)]
    }

    /// Assigns a single value to an address in memory returning a result if the
    /// write was in range.
    fn write(&mut self, addr: u16, value: u8) -> Result<u8, String> {
        let addr_offset = addr - self.start_address;
        self.inner[usize::from(addr_offset)] = value;
        Ok(value)
    }
}

impl Addressable<u16> for Memory<ReadOnly> {
    /// Reads a single byte at the specified address
    fn read(&self, addr: u16) -> u8 {
        let addr_offset = addr - self.start_address;
        self.inner[usize::from(addr_offset)]
    }

    /// write returns an error signifying that the memory is
    /// read-only.
    fn write(&mut self, _: u16, _: u8) -> Result<u8, String> {
        Err("memory is read-only".to_string())
    }
}
