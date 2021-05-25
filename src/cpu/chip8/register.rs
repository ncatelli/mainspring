use crate::cpu::register::Register;

pub trait Decrement {
    fn decrement(self) -> Self;
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum GPRegisters {
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
    VA,
    VB,
    VC,
    VD,
    VE,
    VF,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SpecialRegisters {
    Sound,
    Delay,
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct GeneralPurpose {
    inner: u8,
}

impl Register<u8, u8> for GeneralPurpose {
    fn read(&self) -> u8 {
        self.inner
    }
    fn write(self, value: u8) -> Self {
        Self::with_value(value)
    }

    fn with_value(value: u8) -> Self {
        Self { inner: value }
    }
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct Decrementing {
    inner: GeneralPurpose,
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
