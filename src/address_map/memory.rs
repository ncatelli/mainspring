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
#[derive(Clone, Copy)]
pub struct ReadOnly;

/// Represents a ReadWrite type of memory. This is entirely used for
/// typechecking and has no other practical uses.
#[derive(Clone, Copy)]
pub struct ReadWrite;

/// Represents an addressable segment of memory, be it RAM or ROM.
#[allow(dead_code)]
#[derive(Clone)]
pub struct Memory<T, O, V> {
    mem_type: PhantomData<T>,
    start_address: O,
    stop_address: O,
    inner: Vec<V>,
}

impl<T, O, V> Memory<T, O, V>
where
    O: Into<usize> + Copy,
    V: Default + Clone,
{
    /// Allocates a new addressable memory module taking both a start and stop
    /// address.
    pub fn new(start_address: O, stop_address: O) -> Self {
        let mut data = Vec::new();
        data.resize(
            Into::<usize>::into(stop_address) - Into::<usize>::into(start_address) + 1,
            <V>::default(),
        );
        Memory {
            mem_type: PhantomData,
            start_address,
            stop_address,
            inner: data,
        }
    }

    /// Dump converts the current state of memroy into a correspnding Vec<u8>.
    pub fn dump(&self) -> Vec<V> {
        self.inner.clone()
    }

    /// Load data into memory takes a rom and returns an instance of Memory
    /// with the newly loaded dataset.
    pub fn load(self, data: Vec<V>) -> Self {
        Memory {
            mem_type: self.mem_type,
            start_address: self.start_address,
            stop_address: self.stop_address,
            inner: data,
        }
    }
}

impl Addressable<u16, u8> for Memory<ReadWrite, u16, u8> {
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

impl Addressable<u16, u8> for Memory<ReadOnly, u16, u8> {
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
