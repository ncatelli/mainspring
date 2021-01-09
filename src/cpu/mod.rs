#[cfg(test)]
mod tests;

pub trait Cyclable {
    fn cycles(&self) -> usize {
        1
    }
}

pub trait Offset {
    fn offset(&self) -> usize {
        1
    }
}

pub trait CPU<T> {
    fn step(cpu: T) -> T;
}

pub mod mos6502;
pub mod register;
