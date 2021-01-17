use crate::cpu::register::Register;

/// Represets each type of register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Registers {
    PC,
    ACC,
    X,
    Y,
    PS,
    SP,
}

/// Represets each type of word-sized register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum WordRegisters {
    PC,
}

/// Represets each type of byte-sized register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ByteRegisters {
    ACC,
    X,
    Y,
    PS,
    SP,
}

/// Represets each type of general purpose register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GPRegister {
    ACC,
    X,
    Y,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct StackPointer {
    inner: u8,
}

impl StackPointer {
    /// instantiates a new stack pointer register with a value.
    pub fn with_value(inner: u8) -> Self {
        Self::default().write(inner)
    }
}

impl Register<u16, u8> for StackPointer {
    fn read(&self) -> u16 {
        let bp: u16 = 0x0100;
        bp + self.inner as u16
    }

    fn write(self, value: u8) -> Self {
        Self::with_value(value)
    }

    fn with_value(value: u8) -> Self {
        Self { inner: value }
    }
}

impl Default for StackPointer {
    fn default() -> Self {
        Self { inner: 0xff }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ProcessorStatus {
    pub carry: bool,
    pub zero: bool,
    pub interrupt_disable: bool,
    pub decimal: bool,
    pub brk: bool,
    unused: bool,
    pub overflow: bool,
    pub negative: bool,
}

impl ProcessorStatus {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for ProcessorStatus {
    fn default() -> Self {
        Self {
            carry: false,
            zero: false,
            interrupt_disable: false,
            decimal: false,
            brk: false,
            unused: true,
            overflow: false,
            negative: false,
        }
    }
}

impl Register<u8, u8> for ProcessorStatus {
    fn read(&self) -> u8 {
        self.clone().into()
    }

    fn write(self, _value: u8) -> Self {
        todo!()
    }

    fn with_value(_value: u8) -> Self {
        todo!()
    }
}

impl Into<u8> for ProcessorStatus {
    fn into(self) -> u8 {
        let mut ps: u8 = 0;
        ps |= (self.negative as u8) << 7;
        ps |= (self.overflow as u8) << 6;
        ps |= (self.unused as u8) << 5;
        ps |= (self.brk as u8) << 4;
        ps |= (self.decimal as u8) << 3;
        ps |= (self.interrupt_disable as u8) << 2;
        ps |= (self.zero as u8) << 1;
        ps |= self.carry as u8;
        ps
    }
}
