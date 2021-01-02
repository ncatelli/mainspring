#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct GeneralPurpose {
    inner: u8,
}

impl GeneralPurpose {
    /// instantiates a new GeneralPurpose register with a value.
    pub fn with_value(inner: u8) -> Self {
        GeneralPurpose { inner }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct StackPointer {
    inner: u8,
}

impl StackPointer {
    /// instantiates a new stack pointer register with a value.
    pub fn with_value(inner: u8) -> Self {
        StackPointer { inner }
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
            overflow: false,
            negative: false,
        }
    }
}
