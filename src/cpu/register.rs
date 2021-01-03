pub trait Register<O> {
    fn read(&self) -> O;
    fn write(self, value: u8) -> Self;
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct GeneralPurpose {
    inner: u8,
}

impl Register<u8> for GeneralPurpose {
    fn read(&self) -> u8 {
        self.inner
    }
    fn write(self, value: u8) -> Self {
        Self { inner: value }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct StackPointer {
    inner: u16,
}

impl StackPointer {
    /// instantiates a new stack pointer register with a value.
    pub fn with_value(inner: u8) -> Self {
        Self::default().write(inner)
    }
}

impl Register<u16> for StackPointer {
    fn read(&self) -> u16 {
        self.inner
    }
    fn write(self, value: u8) -> Self {
        let bp: u16 = 0x0100;
        let sp: u16 = bp + value as u16;
        Self { inner: sp }
    }
}

impl Default for StackPointer {
    fn default() -> Self {
        Self { inner: 0xff }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ProcessorStatus {
    carry: bool,
    zero: bool,
    interrupt_disable: bool,
    decimal: bool,
    brk: bool,
    unused: bool,
    overflow: bool,
    negative: bool,
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

impl Register<u8> for ProcessorStatus {
    fn read(&self) -> u8 {
        self.clone().into()
    }
    fn write(self, _value: u8) -> Self {
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
        ps |= (self.carry as u8) << 0;
        ps
    }
}
