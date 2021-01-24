use crate::address_map::{
    memory::{Memory, ReadOnly, ReadWrite},
    Addressable,
};

#[test]
fn should_initialize_memory_to_zeroes() {
    let mem: Memory<ReadOnly> = Memory::new(0, std::u16::MAX);

    assert_eq!(0x00, mem.read(65534))
}

#[test]
fn should_persist_values_in_memory_when_written() {
    let mut mem: Memory<ReadWrite> = Memory::new(0, std::u16::MAX);
    mem.write(0x8000, 0xff).unwrap();

    assert_eq!(0xff, mem.read(0x8000))
}

#[test]
fn should_throw_error_when_write_is_attempted_on_readonly_memory() {
    let mut mem: Memory<ReadOnly> = Memory::new(0, std::u16::MAX);
    assert_eq!(
        Err("memory is read-only".to_string()),
        mem.write(0x8000, 0xff)
    );
}

#[test]
fn should_dump_entire_state_of_memory() {
    let mut expected: Vec<u8> = Vec::new();
    expected.resize(std::u16::MAX as usize + 1, 0);
    expected[0x8000 as usize] = 0xff;

    let mut mem: Memory<ReadWrite> = Memory::new(0, std::u16::MAX);
    mem.write(0x8000, 0xff).unwrap();

    let matches = expected == mem.dump();
    assert!(matches)
}

#[test]
fn should_load_memory_of_correct_size() {
    let mut expected: Vec<u8> = Vec::new();
    expected.resize(std::u16::MAX as usize, 0);
    expected[0x8000 as usize] = 0xff;
    let rom = expected.clone();

    let mem: Memory<ReadWrite> = Memory::new(0, std::u16::MAX).load(rom);

    let matches = expected == mem.dump();
    assert!(matches)
}

#[test]
fn should_correctly_calculate_offsets() {
    let mut mem: Memory<ReadWrite> = Memory::new(0x8000, std::u16::MAX);
    mem.write(0x8000, 0xff).unwrap();

    let data = mem.dump();
    let first_value = data[0];

    assert_eq!(0xff, first_value);
    assert_eq!(0x8000, data.len());
}
