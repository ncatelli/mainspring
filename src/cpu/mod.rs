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
    fn run(self, cycles: usize) -> StepState<T>;
}

/// Stores state between run invocations. The remaining field signifies noop cycles
/// between instructions. This is to function as a placeholder when a run
/// invocation returns while inbetween multi-cycle instructions.
#[derive(Clone)]
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

    /// Decrements the cycle count left on StepState by 1.
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

impl<T> From<T> for StepState<T> {
    fn from(src: T) -> Self {
        StepState::new(1, src)
    }
}

impl<T> From<StepState<T>> for (usize, T) {
    fn from(stepstate: StepState<T>) -> Self {
        (stepstate.remaining, stepstate.cpu)
    }
}

#[macro_use]
pub mod mos6502;
pub mod register;
