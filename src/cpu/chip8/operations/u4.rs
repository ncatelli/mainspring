use std::convert::TryFrom;

#[allow(non_camel_case_types)]
#[derive(Default, Clone, Copy, Debug)]
pub struct u4(u8);

impl u4 {
    pub const MAX: Self = u4(((1 as u8) << 4) - 1);
    pub const MIN: Self = u4(0);
    pub const BITS: u32 = 4;

    fn mask(self) -> Self {
        u4(self.0 & (((1 as u8) << 4).overflowing_sub(1).0))
    }
}

impl u4 {
    /// new instantiates a u4 from a u8 value. Until compile time assertions have been implemented
    pub fn new(value: u8) -> Self {
        TryFrom::try_from(value).unwrap()
    }

    pub const fn min_value() -> u4 {
        u4::MIN
    }
    pub const fn max_value() -> u4 {
        u4::MAX
    }

    pub fn wrapping_sub(self, rhs: Self) -> Self {
        u4(self.0.wrapping_sub(rhs.0)).mask()
    }

    pub fn wrapping_add(self, rhs: Self) -> Self {
        u4(self.0.wrapping_add(rhs.0)).mask()
    }
}

impl PartialEq for u4 {
    fn eq(&self, other: &Self) -> bool {
        self.mask().0 == other.mask().0
    }
}

impl Eq for u4 {}

impl PartialOrd for u4 {
    fn partial_cmp(&self, other: &u4) -> Option<core::cmp::Ordering> {
        self.mask().0.partial_cmp(&other.mask().0)
    }
}

impl Ord for u4 {
    fn cmp(&self, other: &u4) -> core::cmp::Ordering {
        self.mask().0.cmp(&other.mask().0)
    }
}

impl core::fmt::Display for u4 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{}", self.0)
    }
}
impl core::fmt::UpperHex for u4 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:X}", self.0)
    }
}
impl core::fmt::LowerHex for u4 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:x}", self.0)
    }
}
impl core::fmt::Octal for u4 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:o}", self.0)
    }
}
impl core::fmt::Binary for u4 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:b}", self.0)
    }
}

impl<T> core::ops::Shr<T> for u4
where
    u8: core::ops::Shr<T, Output = u8>,
{
    type Output = u4;

    fn shr(self, rhs: T) -> u4 {
        u4(self.mask().0.shr(rhs))
    }
}

impl<T> core::ops::Shl<T> for u4
where
    u8: core::ops::Shl<T, Output = u8>,
{
    type Output = u4;

    fn shl(self, rhs: T) -> u4 {
        u4(self.mask().0.shl(rhs))
    }
}

impl<T> core::ops::ShrAssign<T> for u4
where
    u8: core::ops::ShrAssign<T>,
{
    fn shr_assign(&mut self, rhs: T) {
        *self = self.mask();
        self.0.shr_assign(rhs);
    }
}

impl<T> core::ops::ShlAssign<T> for u4
where
    u8: core::ops::ShlAssign<T>,
{
    fn shl_assign(&mut self, rhs: T) {
        *self = self.mask();
        self.0.shl_assign(rhs);
    }
}

impl core::ops::BitOr<u4> for u4 {
    type Output = u4;

    fn bitor(self, rhs: u4) -> Self::Output {
        u4(self.mask().0.bitor(rhs.mask().0))
    }
}

impl<'a> core::ops::BitOr<&'a u4> for u4 {
    type Output = <u4 as core::ops::BitOr<u4>>::Output;

    fn bitor(self, rhs: &'a u4) -> Self::Output {
        u4(self.mask().0.bitor(rhs.mask().0))
    }
}

impl<'a> core::ops::BitOr<u4> for &'a u4 {
    type Output = <u4 as core::ops::BitOr<u4>>::Output;

    fn bitor(self, rhs: u4) -> Self::Output {
        u4(self.mask().0.bitor(rhs.mask().0))
    }
}

impl<'a> core::ops::BitOr<&'a u4> for &'a u4 {
    type Output = <u4 as core::ops::BitOr<u4>>::Output;

    fn bitor(self, rhs: &'a u4) -> Self::Output {
        u4(self.mask().0.bitor(rhs.mask().0))
    }
}

impl core::ops::BitOrAssign<u4> for u4 {
    fn bitor_assign(&mut self, other: u4) {
        *self = self.mask();
        self.0.bitor_assign(other.mask().0)
    }
}

impl core::ops::Add<u4> for u4 {
    type Output = u4;
    #[allow(unused_comparisons)]
    fn add(self, other: u4) -> u4 {
        if self.0 > 0 && other.0 > 0 {
            debug_assert!(Self::MAX.0 - other.0 >= self.0);
        } else if self.0 < 0 && other.0 < 0 {
            debug_assert!(Self::MIN.0 - other.0 <= self.0);
        }
        self.wrapping_add(other)
    }
}

impl core::ops::Sub<u4> for u4 {
    type Output = u4;
    #[allow(unused_comparisons)]
    fn sub(self, other: u4) -> u4 {
        if self > other {
            debug_assert!(Self::MAX.0 + other.0 >= self.0);
        } else if self < other {
            debug_assert!(Self::MIN.0 + other.0 <= self.0);
        }
        self.wrapping_sub(other)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TryFromIntError;

impl std::fmt::Display for TryFromIntError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "out of range integral type conversion attempted")
    }
}

impl TryFrom<u8> for u4 {
    type Error = TryFromIntError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < u4::MAX.0 {
            Ok(u4(value))
        } else {
            Err(TryFromIntError)
        }
    }
}

impl From<u4> for u8 {
    fn from(src: u4) -> Self {
        src.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Nibbles {
    inner: [u4; 2],
}

impl core::fmt::Display for Nibbles {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{}{}", self.inner[0], self.inner[1])
    }
}
impl core::fmt::UpperHex for Nibbles {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:X}{:X}", self.inner[0], self.inner[1])
    }
}
impl core::fmt::LowerHex for Nibbles {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:x}{:x}", self.inner[0], self.inner[1])
    }
}
impl core::fmt::Octal for Nibbles {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:o}{:o}", self.inner[0], self.inner[1])
    }
}
impl core::fmt::Binary for Nibbles {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "{:b}{:b}", self.inner[0], self.inner[1])
    }
}

impl From<u8> for Nibbles {
    fn from(src: u8) -> Self {
        let upper = TryFrom::try_from((src & 0xf0) >> 4).unwrap();
        let lower = TryFrom::try_from(src & 0x0f).unwrap();

        Self {
            inner: [upper, lower],
        }
    }
}
