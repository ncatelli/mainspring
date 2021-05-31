use crate::cpu::register::Register;

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
#[repr(usize)]
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

/// Represents the special Timer registers that decrement at a pre-selected
/// 60hz (1 second), clock-rate.
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

/// Represents a one of the Special Timer registers that decrements at a set
/// clockrate of 60Hz (1 second).
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ClockDecrementing {
    written_at: std::time::Instant,
    inner: GeneralPurpose<u8>,
}

impl Register<u8, u8> for ClockDecrementing {
    fn read(&self) -> u8 {
        let v = self.inner.read();
        let seconds_since_write = self.written_at.elapsed().as_secs();

        if v as u64 > seconds_since_write {
            std::convert::TryInto::<u8>::try_into(seconds_since_write)
                .map(|ssw| v - ssw)
                .unwrap()
        } else {
            0
        }
    }

    fn write(self, value: u8) -> Self {
        Self {
            written_at: std::time::Instant::now(),
            inner: GeneralPurpose::with_value(value),
        }
    }

    fn with_value(value: u8) -> Self {
        Self {
            written_at: std::time::Instant::now(),
            inner: GeneralPurpose::with_value(value),
        }
    }
}

impl Default for ClockDecrementing {
    fn default() -> Self {
        Self {
            written_at: std::time::Instant::now(),
            inner: GeneralPurpose::default(),
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

    #[test]
    fn should_decrement_clocked_register_at_a_set_pulse() {
        let cdr = ClockDecrementing::with_value(0xff);
        let sleep_time = std::time::Duration::from_secs(2);
        std::thread::sleep(sleep_time);

        assert!(cdr.read() < 0xff)
    }
}
