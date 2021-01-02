#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct GeneralPurpose {
    inner: u8,
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
