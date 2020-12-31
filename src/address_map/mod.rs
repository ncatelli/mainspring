#[cfg(tests)]
mod tests;

type WriteError = String;

/// Addressable implements the trait for addressable memory in an address map.
/// this can represent IO, RAM, ROM, etc...
pub trait Addressable<O: Into<usize>> {
    fn read(offset: O) -> u8;
    fn write(offset: O, data: u8) -> Result<u8, WriteError>;
}
