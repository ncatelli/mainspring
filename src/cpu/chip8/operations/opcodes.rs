use crate::cpu::chip8::operations::u4::u4;
use parcel;

pub fn expect_nibble<'a>(expected: u4) -> impl parcel::Parser<'a, &'a [(usize, u4)], u4> {
    move |input: &'a [(usize, u4)]| match input.get(0) {
        Some(&(pos, next)) if next == expected => Ok(parcel::MatchStatus::Match {
            span: pos..pos + 1,
            remainder: &input[1..],
            inner: next,
        }),
        _ => Ok(parcel::MatchStatus::NoMatch(input)),
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
