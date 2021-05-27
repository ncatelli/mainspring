use parcel::prelude::v1::*;

/// ToNibble provides methods for fetching the upper and lower nibble of a byte.
pub trait ToNibble {
    fn to_upper_nibble(&self) -> u8;
    fn to_lower_nibble(&self) -> u8;
}

impl ToNibble for u8 {
    fn to_upper_nibble(&self) -> u8 {
        (self & 0xf0) >> 4
    }

    fn to_lower_nibble(&self) -> u8 {
        self & 0x0f
    }
}

/// ToNibbles defines a trait for converting a type from a value into its
/// corresponding nibbles.
pub trait ToNibbleBytes {
    fn to_be_nibbles(&self) -> [u8; 2];
    fn to_le_nibbles(&self) -> [u8; 2];
}

impl ToNibbleBytes for u8 {
    fn to_be_nibbles(&self) -> [u8; 2] {
        [self.to_upper_nibble(), self.to_lower_nibble()]
    }

    fn to_le_nibbles(&self) -> [u8; 2] {
        [self.to_lower_nibble(), self.to_upper_nibble()]
    }
}

fn immediate_addressed_opcode<'a>(opcode: u8) -> impl parcel::Parser<'a, &'a [(usize, u8)], u16> {
    parcel::take_n(parcel::parsers::byte::any_byte(), 2)
        .map(|bytes| [bytes[0].to_be_nibbles(), bytes[1].to_be_nibbles()])
        .predicate(move |[first, _]| first[0] == opcode)
        .map(|[[_, first], [second, third]]| {
            let upper = 0x00 | first;
            let lower = (second << 4) | third;
            u16::from_be_bytes([upper, lower])
        })
}

/// Clear the display.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Cls;

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Cls> for Cls {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Cls> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xe0])
            .map(|_| Cls)
            .parse(input)
    }
}

impl From<Cls> for u16 {
    fn from(_: Cls) -> Self {
        0x00e0
    }
}

/// Return from a subroutine.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Ret;

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ret> for Ret {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Ret> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xee])
            .map(|_| Ret)
            .parse(input)
    }
}

impl From<Ret> for u16 {
    fn from(_: Ret) -> Self {
        0x00ee
    }
}

/// Jump to location nnn.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Jp(u16);

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Jp> for Jp {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Jp> {
        immediate_addressed_opcode(0x01)
            .map(|addr| Jp(addr))
            .parse(input)
    }
}

impl From<Jp> for u16 {
    fn from(src: Jp) -> Self {
        0x1000 | src.0
    }
}

/// Call subroutine at nnn.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Call(u16);

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Call> for Call {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Call> {
        immediate_addressed_opcode(0x02)
            .map(|addr| Call(addr))
            .parse(input)
    }
}

impl From<Call> for u16 {
    fn from(src: Call) -> Self {
        0x2000 | src.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_cls_opcode() {
        let input: Vec<(usize, u8)> = 0x00e0u16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Cls
            }),
            Cls.parse(&input[..])
        );
    }

    #[test]
    fn should_parse_ret_opcode() {
        let input: Vec<(usize, u8)> = 0x00eeu16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Ret
            }),
            Ret::default().parse(&input[..])
        );
    }

    #[test]
    fn should_parse_jump_opcode() {
        let input: Vec<(usize, u8)> = 0x1fffu16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Jp(0x0fff)
            }),
            Jp::default().parse(&input[..])
        );
    }

    #[test]
    fn should_parse_call_opcode() {
        let input: Vec<(usize, u8)> = 0x2fffu16
            .to_be_bytes()
            .iter()
            .copied()
            .enumerate()
            .collect();
        assert_eq!(
            Ok(MatchStatus::Match {
                span: 0..2,
                remainder: &input[2..],
                inner: Call(0x0fff)
            }),
            Call::default().parse(&input[..])
        );
    }
}
