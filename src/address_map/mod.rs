use std::collections::HashMap;
use std::fmt;
use std::{cmp::Eq, fmt::Debug, hash::Hash, ops::RangeInclusive};

pub mod memory;
pub mod page;

#[cfg(test)]
mod tests;

type WriteError = String;
type RegistrationError = String;

/// SafeAddressable represents an implementation of Addressable that will
/// _NEVER_ fail a write. An example would be a ring buffer that will always
/// wrap its address space.
pub trait SafeAddressable<O, V>
where
    Self: Addressable<O, V>,
    O: Into<usize> + Debug + Clone + Copy,
    V: Debug + Clone + Copy,
{
    fn read(&self, offset: O) -> V {
        Addressable::<O, V>::read(self, offset)
    }

    fn write(&mut self, offset: O, data: V) -> V {
        Addressable::<O, V>::write(self, offset, data).unwrap()
    }
}

/// Addressable implements the trait for addressable memory in an address map.
/// this can represent IO, RAM, ROM, etc...
pub trait Addressable<O, V>
where
    Self: AddressableClone<O, V>,
    O: Into<usize> + Debug + Clone + Copy,
    V: Debug + Clone + Copy,
{
    fn read(&self, offset: O) -> V;
    fn write(&mut self, offset: O, data: V) -> Result<V, WriteError>;
}

impl<O, V> Clone for Box<dyn Addressable<O, V>>
where
    O: Into<usize> + Debug + Clone + Copy,
{
    fn clone(&self) -> Box<dyn Addressable<O, V>> {
        self.clone_box()
    }
}

pub trait AddressableClone<O, V> {
    fn clone_box(&self) -> Box<dyn Addressable<O, V>>;
}

impl<T, O, V> AddressableClone<O, V> for T
where
    T: 'static + Addressable<O, V> + Clone,
    O: Into<usize> + Debug + Clone + Copy,
    V: Debug + Clone + Copy,
{
    fn clone_box(&self) -> Box<dyn Addressable<O, V>> {
        Box::new(self.clone())
    }
}

/// AddressMap contains a mapping of address spaces to corresponding addressable
/// IO with the purpose of acting as an address map. This time is, additionally,
/// an implementation Addressable allowing all other components to interact with
/// it as if it were a bus.
#[derive(Default, Clone)]
pub struct AddressMap<O, V>
where
    O: Into<usize> + Debug + Clone + Copy,
{
    inner: HashMap<RangeInclusive<O>, Box<dyn Addressable<O, V>>>,
}

impl<O, V> fmt::Debug for AddressMap<O, V>
where
    O: Into<usize> + Debug + Clone + Copy,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let keys: Vec<&RangeInclusive<O>> = self.inner.keys().collect();
        write!(f, "AddressMap {:?}", keys)
    }
}

impl<O, V> AddressMap<O, V>
where
    O: Into<usize> + Hash + PartialOrd + Eq + Debug + Clone + Copy,
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
        range: RangeInclusive<O>,
        addr_space: Box<dyn Addressable<O, V>>,
    ) -> Result<AddressMap<O, V>, RegistrationError> {
        self.inner
            .keys()
            .map(|key| {
                if key.contains(range.start()) || key.contains(range.end()) {
                    Err(format!(
                        "address space {:?} overlaps with {:?}",
                        &range, &key
                    ))
                } else {
                    Ok(())
                }
            })
            .collect::<Result<Vec<()>, RegistrationError>>()
            .map(|_| {
                self.inner.insert(range, addr_space);
                self
            })
    }
}

impl<O, V> Addressable<O, V> for AddressMap<O, V>
where
    O: 'static + Into<usize> + Hash + PartialOrd + Eq + Debug + Clone + Copy,
    V: 'static + Default + Debug + Clone + Copy,
{
    /// Reads a single byte at the specified address
    fn read(&self, addr: O) -> V {
        self.inner
            .keys()
            .filter(|key| key.contains(&addr))
            .flat_map(|r| self.inner.get(r))
            .next()
            .map_or(<V>::default(), |a| a.read(addr))
    }

    /// Write assigns a single value to an address in memory
    fn write(&mut self, addr: O, value: V) -> Result<V, String> {
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
