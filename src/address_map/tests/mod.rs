use crate::address_map::{
    memory::{Memory, ReadOnly},
    AddressMap,
};

mod memory;

#[test]
fn should_register_valid_memory() {
    let addr_space = 0..std::u16::MAX;
    let mem: Memory<ReadOnly> = Memory::new(addr_space.start, addr_space.end);
    assert!(AddressMap::<u16>::new()
        .register(addr_space, Box::new(mem))
        .is_ok());
}
