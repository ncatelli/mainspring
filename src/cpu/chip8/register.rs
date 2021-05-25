use crate::cpu::register::Register;

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
