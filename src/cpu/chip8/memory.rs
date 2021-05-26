use crate::address_map::{Addressable, SafeAddressable};

#[derive(Default, Debug, Clone)]
pub struct Ring<T> {
    capacity: usize,
    inner: Vec<T>,
}

impl<T> Ring<T>
where
    T: Default,
{
    pub fn new(capacity: usize) -> Self {
        let inner = (0..capacity).into_iter().map(|_| <T>::default()).collect();
        Self {
            capacity,
            inner: inner,
        }
    }
}

impl<T> Addressable<usize, T> for Ring<T>
where
    T: 'static + Default + std::fmt::Debug + Copy,
{
    fn read(&self, offset: usize) -> T {
        let wrapped_offset = offset % self.capacity;

        self.inner
            .get(wrapped_offset)
            .map_or(<T>::default(), |v| *v)
    }

    fn write(&mut self, offset: usize, data: T) -> Result<T, String> {
        let wrapped_offset = offset % self.capacity;

        self.inner
            .get_mut(wrapped_offset)
            .map(|v| {
                *v = data;
                data
            })
            .ok_or(format!("invalid addr: {}", offset))
    }
}

impl<T> SafeAddressable<usize, T> for Ring<T>
where
    Self: Addressable<usize, T>,
    T: 'static + Default + std::fmt::Debug + Copy,
{
    // inherit default implementation
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address_map::SafeAddressable;
    #[test]
    fn should_wrap_on_overflow() {
        let mut ring = Ring::<u8>::new(4);
        assert_eq!(0x00, SafeAddressable::read(&ring, 0));

        SafeAddressable::write(&mut ring, 0, 0xff);
        assert_eq!(0xff, SafeAddressable::read(&ring, 4));
        assert_eq!(0xff, SafeAddressable::read(&ring, 16));
        assert_eq!(0x00, SafeAddressable::read(&ring, 17));
    }
}
