use std::collections::HashMap;
use std::ops::Range;

pub mod memory;

#[cfg(test)]
mod tests;

type WriteError = String;

/// Addressable implements the trait for addressable memory in an address map.
/// this can represent IO, RAM, ROM, etc...
pub trait Addressable<O: Into<usize>> {
    fn read(&self, offset: O) -> u8;
    fn write(&mut self, offset: O, data: u8) -> Result<u8, WriteError>;
}

/// AddressMap
pub struct AddressMap<O: Into<usize>> {
    map: HashMap<Range<O>, Box<dyn Addressable<O>>>,
}

impl<O> AddressMap<O>
where
    O: Into<usize>,
{
    pub fn new() -> Self {
        AddressMap {
            map: HashMap::new(),
        }
    }
}
