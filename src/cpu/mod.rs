#[cfg(test)]
mod tests;

pub trait CPU<T> {
    fn step(cpu: T) -> T;
}

pub mod mos6502;
pub mod register;
