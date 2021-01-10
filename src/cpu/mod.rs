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
    fn step(self) -> StepState<T>;
}

/// Stores state between cycles.
pub struct StepState<T> {
    remaining: usize, // Remaining cycles in operation
    cpu: T,
}

impl<T> StepState<T> {
    pub fn new(cycle: usize, cpu: T) -> Self {
        Self {
            remaining: cycle - 1,
            cpu,
        }
    }

    pub fn decrement(self) -> Self {
        let remaining = self.remaining - 1;
        Self {
            remaining,
            cpu: self.cpu,
        }
    }

    pub fn ready(&self) -> bool {
        self.remaining == 0
    }

    pub fn unwrap(self) -> T {
        self.cpu
    }
}

impl<T> From<StepState<T>> for (usize, T) {
    fn from(stepstate: StepState<T>) -> Self {
        (stepstate.remaining, stepstate.cpu)
    }
}

pub mod mos6502;
pub mod register;
