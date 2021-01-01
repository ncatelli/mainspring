use std::collections::HashMap;
use std::{cmp::Eq, fmt::Debug, hash::Hash, ops::Range};

pub mod memory;

#[cfg(test)]
mod tests;

type WriteError = String;
type RegistrationError = String;

/// Addressable implements the trait for addressable memory in an address map.
/// this can represent IO, RAM, ROM, etc...
pub trait Addressable<O>
where
    O: Into<usize> + Debug,
{
    fn read(&self, offset: O) -> u8;
    fn write(&mut self, offset: O, data: u8) -> Result<u8, WriteError>;
}

/// AddressMap contains a mapping of address spaces to corresponding addressable
/// IO with the purpose of acting as an address map. This time is, additionally,
/// an implementation Addressable allowing all other components to interact with
/// it as if it were a bus.
#[derive(Default)]
pub struct AddressMap<O: Into<usize>> {
    inner: HashMap<Range<O>, Box<dyn Addressable<O>>>,
}

impl<O> AddressMap<O>
where
    O: Into<usize> + Hash + PartialOrd + Eq + Debug,
{
    pub fn new() -> Self {
        AddressMap {
            inner: HashMap::default(),
        }
    }

    /// register attempts takes a range, representing a range of addresses and
    /// an addressable type for receiving read/write requests.
    pub fn register(
        mut self,
        range: Range<O>,
        addr_space: Box<dyn Addressable<O>>,
    ) -> Result<AddressMap<O>, RegistrationError> {
        self.inner
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
                self.inner.insert(range, addr_space);
                self
            })
    }
}

impl<T> Addressable<T> for AddressMap<T>
where
    T: Into<usize> + Hash + PartialOrd + Eq + Debug + Copy,
{
    /// Reads a single byte at the specified address
    fn read(&self, addr: T) -> u8 {
        self.inner
            .keys()
            .filter(|key| key.contains(&addr))
            .map(|r| self.inner.get(r))
            .flatten()
            .next()
            .map_or(0x00, |a| a.read(addr))
    }

    /// Write assigns a single value to an address in memory
    fn write(&mut self, addr: T, value: u8) -> Result<u8, String> {
        let range = self
            .inner
            .keys()
            .cloned()
            .find(|key| key.contains(&addr))
            .ok_or(format!("address space {:?} unallocated", addr))?;
        let am = self
            .inner
            .get_mut(&range)
            .ok_or(format!("address space {:?} unallocated", addr))?;
        am.write(addr, value)
    }
}
