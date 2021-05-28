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

pub trait Generate<T, U> {
    fn generate(self, cpu: &T) -> U;
}

pub trait Execute<T> {
    fn execute(self, cpu: T) -> T;
}

pub trait Cpu<T> {
    fn run(self, cycles: usize) -> StepState<T>;
}

/// Stores state between run invocations. The remaining field signifies noop cycles
/// between instructions. This is to function as a placeholder when a run
/// invocation returns while inbetween multi-cycle instructions.
pub enum StepState<T> {
    NotReady(usize, T),
    Ready(T),
}

impl<T> StepState<T> {
    pub fn new(cycle: usize, state: T) -> Self {
        match cycle {
            0 | 1 => Self::Ready(state),
            _ => Self::NotReady(cycle - 1, state),
        }
    }

    /// Decrements the cycle count left on StepState by 1.
    pub fn decrement(self) -> Self {
        match self {
            ss @ Self::Ready(_) => ss,
            Self::NotReady(1, state) => Self::Ready(state),
            Self::NotReady(0, state) => Self::Ready(state),
            Self::NotReady(cycles, state) => Self::NotReady(cycles - 1, state),
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Self::Ready(state) => state,
            Self::NotReady(_, state) => state,
        }
    }
}

impl<T> From<T> for StepState<T> {
    fn from(src: T) -> Self {
        StepState::new(1, src)
    }
}

impl<T> From<StepState<T>> for (usize, T) {
    fn from(stepstate: StepState<T>) -> Self {
        match stepstate {
            StepState::Ready(state) => (0, state),
            StepState::NotReady(cycles, state) => (cycles, state),
        }
    }
}

#[macro_use]
pub mod mos6502;
pub mod chip8;
pub mod register;
