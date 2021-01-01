use crate::address_map::Addressable;

mod memory;

macro_rules! u16_addresss_map {
    () => {
        $crate::address_map::AddressMap::<u16>::new()
            .register(
                0..std::u16::MAX,
                Box::new($crate::address_map::memory::Memory::<
                    $crate::address_map::memory::ReadOnly,
                >::new(0, std::u16::MAX)),
            )
            .unwrap()
    };
}

#[test]
fn should_read_valid_memory() {
    let am = u16_addresss_map!();
    assert_eq!(0x00, am.read(0xffff));
}

#[test]
fn should_write_valid_memory() {
    let mut am = u16_addresss_map!();
    assert!(am.write(0xaaaa, 0xff).is_ok());
    assert_eq!(0xff, am.read(0xaaaa));
}
