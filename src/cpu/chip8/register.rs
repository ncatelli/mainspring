use crate::cpu::register::Register;

pub trait Decrement {
    fn decrement(self) -> Self;
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ByteRegisters {
    GpRegisters(GpRegisters),
    TimerRegisters(TimerRegisters),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum WordRegisters {
    I,
    ProgramCounter,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum GpRegisters {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    V8,
    V9,
    Va,
    Vb,
    Vc,
    Vd,
    Ve,
    Vf,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TimerRegisters {
    Sound,
    Delay,
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct GeneralPurpose<T> {
    inner: T,
}

impl<T> Register<T, T> for GeneralPurpose<T>
where
    T: Copy,
{
    fn read(&self) -> T {
        self.inner
    }
    fn write(self, value: T) -> Self {
        Self::with_value(value)
    }

    fn with_value(value: T) -> Self {
        Self { inner: value }
    }
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct Decrementing {
    inner: GeneralPurpose<u8>,
}

impl Decrement for Decrementing {
    fn decrement(self) -> Self {
        let old_val = self.read();
        self.write(old_val - 1)
    }
}

impl Register<u8, u8> for Decrementing {
    fn read(&self) -> u8 {
        self.inner.read()
    }
    fn write(self, value: u8) -> Self {
        Self {
            inner: GeneralPurpose::with_value(value),
        }
    }

    fn with_value(value: u8) -> Self {
        Self {
            inner: GeneralPurpose::with_value(value),
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct ProgramCounter {
    inner: u16,
}

impl Register<u16, u16> for ProgramCounter {
    fn read(&self) -> u16 {
        self.inner
    }
    fn write(self, value: u16) -> Self {
        Self::with_value(value)
    }

    fn with_value(value: u16) -> Self {
        Self { inner: value }
    }
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct StackPointer {
    inner: u8,
}

impl Register<u8, u8> for StackPointer {
    fn read(&self) -> u8 {
        value_to_sp(self.inner)
    }
    fn write(self, value: u8) -> Self {
        Self::with_value(value_to_sp(value))
    }

    fn with_value(value: u8) -> Self {
        Self {
            inner: value_to_sp(value),
        }
    }
}

const fn value_to_sp(value: u8) -> u8 {
    value % 16
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_generate_valid_ranged_value_for_stackpointer() {
        for valid_addr in 0..16 {
            assert_eq!(valid_addr, StackPointer::with_value(valid_addr).read());
        }

        // assert overflow rolls around
        assert_eq!(0, StackPointer::with_value(16).read());
        assert_eq!(1, StackPointer::with_value(17).read());
    }
}
