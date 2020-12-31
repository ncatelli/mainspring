use std::collections::HashMap;
use std::{cmp::Eq, fmt::Debug, hash::Hash, ops::Range};

pub mod memory;

#[cfg(test)]
mod tests;

type WriteError = String;
type RegistrationError = String;

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
    O: Into<usize> + Hash + PartialOrd + Eq + Debug,
{
    pub fn new() -> Self {
        AddressMap {
            map: HashMap::new(),
        }
    }

    /// register attempts to match a new range
    pub fn register(
        mut self,
        range: Range<O>,
        addr_space: Box<dyn Addressable<O>>,
    ) -> Result<AddressMap<O>, RegistrationError> {
        self.map
            .keys()
            .map(|key| {
                if key.contains(&range.start) || key.contains(&range.end) {
                    Err(format!(
                        "address space {:?} overlaps with {:?}",
                        &range, &key
                    ))
                } else {
                    Ok(())
                }
            })
            .collect::<Result<Vec<()>, RegistrationError>>()
            .map_err(|e| e)
            .map(|_| {
                self.map.insert(range, addr_space);
                self
            })
    }
}
