use crate::addressable::{Memory, ReadOnly, ReadWrite};

#[test]
fn should_initialize_memory_to_zeroes() {
    let mem: Memory<ReadOnly> = Memory::new(0, std::u16::MAX);

    assert_eq!(0x00, mem.read(65534))
}

#[test]
fn should_persist_values_in_memory_when_written() {
    let mut mem: Memory<ReadWrite> = Memory::new(0, std::u16::MAX);
    mem.write(0xff, 0x8000);

    assert_eq!(0xff, mem.read(0x8000))
}
