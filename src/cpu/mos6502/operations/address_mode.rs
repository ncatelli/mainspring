extern crate parcel;
use crate::cpu::{Cyclable, Offset};
use parcel::{parsers::byte::any_byte, MatchStatus, ParseResult, Parser};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Accumulator;

/// Implied address address mode. This is signified by no address mode
/// arguments. An example instruction with an implied address mode would be.
/// `nop`
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Implied;

impl Offset for Implied {
    fn offset(&self) -> usize {
        0
    }
}

impl<'a> Parser<'a, &'a [u8], Implied> for Implied {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Implied> {
        Ok(MatchStatus::Match((input, Implied)))
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Immediate(pub u8);

impl Cyclable for Immediate {}
impl Offset for Immediate {}

impl<'a> Parser<'a, &'a [u8], Immediate> for Immediate {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Immediate> {
        any_byte().map(Immediate).parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Absolute(pub u16);

impl Offset for Absolute {
    fn offset(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], Absolute> for Absolute {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Absolute> {
        parcel::take_n(any_byte(), 2)
            .map(|b| Absolute(u16::from_le_bytes([b[0], b[1]])))
            .parse(input)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ZeroPage(pub u8);

impl Cyclable for ZeroPage {}
impl Offset for ZeroPage {}

impl<'a> Parser<'a, &'a [u8], ZeroPage> for ZeroPage {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], ZeroPage> {
        any_byte().map(ZeroPage).parse(input)
    }
}

impl ZeroPage {
    /// Unpacks the enclosed address from a Zeropage into a corresponding u8
    /// address.
    pub fn unwrap(self) -> u8 {
        self.into()
    }
}

impl From<ZeroPage> for u8 {
    fn from(src: ZeroPage) -> Self {
        src.0
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ZeroPageIndexedWithX(pub u8);

impl Cyclable for ZeroPageIndexedWithX {}
impl Offset for ZeroPageIndexedWithX {}

impl<'a> Parser<'a, &'a [u8], ZeroPageIndexedWithX> for ZeroPageIndexedWithX {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], ZeroPageIndexedWithX> {
        any_byte().map(ZeroPageIndexedWithX).parse(input)
    }
}

impl ZeroPageIndexedWithX {
    /// Unpacks the enclosed address from a ZeropageIndexedWithX into a
    /// corresponding u8 address.
    pub fn unwrap(self) -> u8 {
        self.into()
    }
}

impl From<ZeroPageIndexedWithX> for u8 {
    fn from(src: ZeroPageIndexedWithX) -> Self {
        src.0
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct ZeroPageIndexedWithY(pub u8);

impl Cyclable for ZeroPageIndexedWithY {}
impl Offset for ZeroPageIndexedWithY {}

impl<'a> Parser<'a, &'a [u8], ZeroPageIndexedWithY> for ZeroPageIndexedWithY {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], ZeroPageIndexedWithY> {
        any_byte().map(ZeroPageIndexedWithY).parse(input)
    }
}

/// Relative wraps an i8 and signifies a relative address to the current
/// instruction. This is commonly used alongside branch instructions that may
/// cause a short jump either forward or back in memory to facilitate looping.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Relative(pub i8);

impl Offset for Relative {}

impl<'a> Parser<'a, &'a [u8], Relative> for Relative {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Relative> {
        any_byte()
            .map(|b| {
                let offset = unsafe { std::mem::transmute::<u8, i8>(b) };
                Relative(offset)
            })
            .parse(input)
    }
}

impl Relative {
    /// Unpacks the enclosed address from a Relative addressmode into a
    /// corresponding i8 address.
    pub fn unwrap(self) -> i8 {
        self.into()
    }
}

impl From<Relative> for i8 {
    fn from(src: Relative) -> Self {
        src.0
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Indirect(pub u16);

impl Offset for Indirect {
    fn offset(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], Indirect> for Indirect {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], Indirect> {
        parcel::take_n(any_byte(), 2)
            .map(|b| Indirect(u16::from_le_bytes([b[0], b[1]])))
            .parse(input)
    }
}

/// AbsoluteIndexedWithX represents an address whose value is stored at an X
/// register offset from the operand value. Example being LLHH + X.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct AbsoluteIndexedWithX(pub u16);

impl Offset for AbsoluteIndexedWithX {
    fn offset(&self) -> usize {
        2
    }
}

impl AbsoluteIndexedWithX {
    /// Unpacks the enclosed address from a AbsoluteIndexedWithX address mode
    /// into a corresponding u16 address.
    pub fn unwrap(self) -> u16 {
        self.into()
    }
}

impl From<AbsoluteIndexedWithX> for u16 {
    fn from(src: AbsoluteIndexedWithX) -> Self {
        src.0
    }
}

impl<'a> Parser<'a, &'a [u8], AbsoluteIndexedWithX> for AbsoluteIndexedWithX {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], AbsoluteIndexedWithX> {
        parcel::take_n(any_byte(), 2)
            .map(|b| AbsoluteIndexedWithX(u16::from_le_bytes([b[0], b[1]])))
            .parse(input)
    }
}

/// AbsoluteIndexedWithY represents an address whose value is stored at an Y
/// register offset from the operand value. Example being LLHH + Y.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct AbsoluteIndexedWithY(pub u16);

impl Offset for AbsoluteIndexedWithY {
    fn offset(&self) -> usize {
        2
    }
}

impl<'a> Parser<'a, &'a [u8], AbsoluteIndexedWithY> for AbsoluteIndexedWithY {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], AbsoluteIndexedWithY> {
        parcel::take_n(any_byte(), 2)
            .map(|b| AbsoluteIndexedWithY(u16::from_le_bytes([b[0], b[1]])))
            .parse(input)
    }
}

impl AbsoluteIndexedWithY {
    /// Unpacks the enclosed address from a AbsoluteIndexedWithY address mode
    /// into a corresponding u16 address.
    pub fn unwrap(self) -> u16 {
        self.into()
    }
}

impl From<AbsoluteIndexedWithY> for u16 {
    fn from(src: AbsoluteIndexedWithY) -> Self {
        src.0
    }
}

/// XIndexedIndirect represents an address whose value is stored as an X
/// register offset for sequential bytes of an address word. Example being
/// LL + X, LL + X + 1.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct XIndexedIndirect(pub u8);

impl Offset for XIndexedIndirect {}

impl<'a> Parser<'a, &'a [u8], XIndexedIndirect> for XIndexedIndirect {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], XIndexedIndirect> {
        any_byte().map(XIndexedIndirect).parse(input)
    }
}

impl XIndexedIndirect {
    /// Unpacks the enclosed address from a XIndexedIndirect address mode into
    /// a corresponding u address.
    pub fn unwrap(self) -> u8 {
        self.into()
    }
}

impl From<XIndexedIndirect> for u8 {
    fn from(src: XIndexedIndirect) -> Self {
        src.0
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct IndirectYIndexed(pub u8);

impl Offset for IndirectYIndexed {}

impl<'a> Parser<'a, &'a [u8], IndirectYIndexed> for IndirectYIndexed {
    fn parse(&self, input: &'a [u8]) -> ParseResult<&'a [u8], IndirectYIndexed> {
        any_byte().map(IndirectYIndexed).parse(input)
    }
}
