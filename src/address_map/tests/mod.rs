use crate::address_map::{
    memory::{Memory, ReadOnly},
    Addressable,
};

mod memory;

macro_rules! u16_address_map {
    () => {
        $crate::address_map::AddressMap::<u16, u8>::new().register(
            0..=std::u16::MAX,
            Box::new($crate::address_map::memory::Memory::<
                $crate::address_map::memory::ReadWrite,
                u16,
                u8,
            >::new(0, std::u16::MAX)),
        )
    };
    ($am:expr) => {
        $crate::address_map::AddressMap::<u16>::new().register(0..=std::u16::MAX, Box::new($am))
    };
    ($range:expr, $am:expr) => {
        $crate::address_map::AddressMap::<u16, u8>::new().register($range, Box::new($am))
    };
}

#[test]
fn should_register_valid_memory() {
    let am = u16_address_map!(
        0..=std::u16::MAX,
        Memory::<ReadOnly, u16, u8>::new(0, std::u16::MAX)
    );
    assert!(am.is_ok());
}

#[test]
fn should_fail_when_registering_overlapping_address_space() {
    let am = u16_address_map!(0..=0x7fff, Memory::<ReadOnly, u16, u8>::new(0, 0x7fff)).unwrap();
    assert!(am
        .register(
            0..=0x7fff,
            Box::new(Memory::<ReadOnly, u16, u8>::new(0, 0x7fff))
        )
        .is_err());
}

#[test]
fn should_read_valid_memory() {
    let am = u16_address_map!().unwrap();
    assert_eq!(0x00, am.read(0xffff));
}

#[test]
fn should_write_valid_memory() {
    let mut am = u16_address_map!().unwrap();
    assert!(am.write(0xaaaa, 0xff).is_ok());
    assert_eq!(0xff, am.read(0xaaaa));
}
