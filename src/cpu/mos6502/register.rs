use crate::cpu::register::Register;

/// Represets each type of register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Registers {
    Pc,
    Acc,
    X,
    Y,
    Ps,
    Sp,
}

/// Represets each type of word-sized register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WordRegisters {
    Pc,
}

/// Represets each type of byte-sized register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteRegisters {
    Acc,
    X,
    Y,
    Ps,
    Sp,
}

/// Represets each flag represented in the ProgramStatus Register.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProgramStatusFlags {
    Negative,
    Overflow,
    Break,
    Decimal,
    Interrupt,
    Zero,
    Carry,
}

/// Represets each type of general purpose register available in the mos6502.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GpRegister {
    Acc,
    X,
    Y,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackPointer {
    inner: u8,
}

impl Register<u8, u8> for StackPointer {
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

impl Default for StackPointer {
    fn default() -> Self {
        Self { inner: 0xff }
    }
}

/// bit_is_set takes a u8 value and a u8 representing the bit place returning a
/// bool if the place is set. This defaults to false if it is out of range.
const fn bit_is_set(value: u8, place: u8) -> bool {
    if place > 7 {
        false
    } else {
        ((value >> place) & 1) == 1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
        (*self).into()
    }

    fn write(self, value: u8) -> Self {
        Self::with_value(value)
    }

    fn with_value(value: u8) -> Self {
        // check if each bit is set
        Self {
            negative: bit_is_set(value, 7),
            overflow: bit_is_set(value, 6),
            unused: bit_is_set(value, 5),
            brk: bit_is_set(value, 4),
            decimal: bit_is_set(value, 3),
            interrupt_disable: bit_is_set(value, 2),
            zero: bit_is_set(value, 1),
            carry: bit_is_set(value, 0),
        }
    }
}

impl From<ProcessorStatus> for u8 {
    fn from(src: ProcessorStatus) -> u8 {
        // Convert a bool to a u8 (0 or 1) and shift it to it's corresponding
        //place.
        let mut ps: u8 = 0;
        ps |= (src.negative as u8) << 7;
        ps |= (src.overflow as u8) << 6;
        ps |= (src.unused as u8) << 5;
        ps |= (src.brk as u8) << 4;
        ps |= (src.decimal as u8) << 3;
        ps |= (src.interrupt_disable as u8) << 2;
        ps |= (src.zero as u8) << 1;
        ps |= src.carry as u8;
        ps
    }
}
