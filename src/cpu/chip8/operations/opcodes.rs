/// ToNibbles defines a trait for converting a type from a value into its
/// corresponding nibbles.
pub trait ToNibbles {
    fn to_nibbles(&self) -> [u8; 2];
}

impl ToNibbles for u8 {
    fn to_nibbles(&self) -> [u8; 2] {
        let upper = (self & 0xf0) >> 4;
        let lower = self & 0x0f;

        [upper, lower]
    }
}

/// Clear the display.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Cls;

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Cls> for Cls {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Cls> {
        parcel::map(
            parcel::join(
                parcel::parsers::byte::expect_byte(0x00),
                parcel::parsers::byte::expect_byte(0xe0),
            ),
            |_| Cls::default(),
        )
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
        parcel::map(
            parcel::join(
                parcel::parsers::byte::expect_byte(0x00),
                parcel::parsers::byte::expect_byte(0xee),
            ),
            |_| Ret::default(),
        )
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
        parcel::take_n(parcel::parsers::byte::any_byte(), 2)
            .map(|bytes| [bytes[0].to_nibbles(), bytes[1].to_nibbles()])
            .predicate(|[first, _]| first[0] == 0x1u8)
            .map(|[[_, first], [second, third]]| {
                let upper = 0x00 | first;
                let lower = (second << 4) | third;
                Jp(u16::from_be_bytes([upper, lower]))
            })
            .parse(input)
    }
}

impl From<Jp> for u16 {
    fn from(src: Jp) -> Self {
        0x1000 | src.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parcel::prelude::v1::*;

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
}
