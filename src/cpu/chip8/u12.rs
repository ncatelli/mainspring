/// Represents an unsigned 12-bit integer.
#[derive(Default, Clone, Copy)]
#[allow(non_camel_case_types)]
pub struct u12(u16);

impl u12 {
    /// The largest value of the u12 integer.
    pub const MAX: Self = Self(0xFFF);
    /// The smallest value of the u12 integer.
    pub const MIN: Self = Self(0x000);
    /// The number of bits in the u12 integer.
    pub const BITS: u32 = 12;
}

impl u12 {
    /// Takes a 3 value array representing the nibblees in a big-endian format
    /// and attempts to covert the nibbles to a corresponding u12. All nibbles
    /// are masked and will be truncated to a maximum value of 0x0f.
    pub fn from_be_nibbles(src: [u8; 3]) -> u12 {
        let masked_src = [src[0] & 0x0f, src[1] & 0x0f, src[2] & 0x0f];
        u12::from(masked_src)
    }
}

impl u12 {
    /// Instantiates a new u12.
    pub fn new(value: u16) -> Self {
        if value > Self::MAX.0 {
            panic!("this value will overflow")
        } else {
            Self(value)
        }
    }

    pub fn wrapping_add(self, rhs: Self) -> Self {
        let (lhs, rhs) = (self.0, rhs.0);

        u12::new(lhs.wrapping_add(rhs).mask())
    }

    pub fn wrapping_sub(self, rhs: Self) -> Self {
        let (lhs, rhs) = (self.0, rhs.0);

        u12::new(lhs.wrapping_sub(rhs).mask())
    }
}

impl core::fmt::Debug for u12 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
        <u16 as core::fmt::Debug>::fmt(&self.0, f)
    }
}

impl core::fmt::Display for u12 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
        <u16 as core::fmt::Display>::fmt(&self.0, f)
    }
}

impl core::fmt::UpperHex for u12 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> core::fmt::Result {
        <u16 as core::fmt::UpperHex>::fmt(&self.0, f)
    }
}

impl core::fmt::LowerHex for u12 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        <u16 as core::fmt::LowerHex>::fmt(&self.0, f)
    }
}

impl core::fmt::Octal for u12 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        <u16 as core::fmt::Octal>::fmt(&self.0, f)
    }
}

impl core::fmt::Binary for u12 {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        <u16 as core::fmt::Binary>::fmt(&self.0, f)
    }
}

impl core::cmp::PartialEq for u12 {
    fn eq(&self, other: &Self) -> bool {
        self.0.mask() == other.0.mask()
    }
}

impl Eq for u12 {}

impl core::cmp::PartialOrd for u12 {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.0.mask().partial_cmp(&other.0.mask())
    }
}

impl core::cmp::Ord for u12 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.mask().cmp(&other.0.mask())
    }
}

impl core::ops::Add for u12 {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.0, other.0);
        let sum = lhs + rhs;

        if sum > Self::MAX.0 {
            panic!("this arithmetic operation will overflow")
        } else {
            Self::new(sum)
        }
    }
}

impl core::ops::Sub for u12 {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.0, other.0);
        Self(lhs - rhs).mask()
    }
}

impl core::ops::BitAnd for u12 {
    type Output = Self;

    fn bitand(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.0, other.0);
        Self(lhs & rhs).mask()
    }
}

impl core::ops::BitOr for u12 {
    type Output = Self;

    fn bitor(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.0, other.0);
        Self(lhs | rhs).mask()
    }
}

impl core::ops::Shl for u12 {
    type Output = Self;

    fn shl(self, other: Self) -> Self {
        let (lhs, rhs) = (self.0, other.0);
        Self(lhs << rhs).mask()
    }
}

impl core::ops::Shr for u12 {
    type Output = Self;

    fn shr(self, other: Self) -> Self {
        let (lhs, rhs) = (self.0, other.0);
        Self(lhs >> rhs).mask()
    }
}

macro_rules! impl_from_u12_for_uX {
    ($($t:ty,)*) => {
        $(
            impl From<u12> for $t {
                fn from(src: u12) -> Self {
                    src.0 as Self
                }
            }
        )*
    };
}

impl_from_u12_for_uX!(u16, u32, u64, u128,);

impl From<[u8; 3]> for u12 {
    fn from(src: [u8; 3]) -> Self {
        let msb = src[0] & 0x0f;
        let lsb = ((src[1] & 0x0f) << 4) | (src[2] & 0x0f);
        let val = u16::from_be_bytes([msb, lsb]);

        u12::new(val)
    }
}

trait MaskU12 {
    fn mask(self) -> Self;
}

impl MaskU12 for u12 {
    fn mask(self) -> u12 {
        u12::new(self.0.mask())
    }
}

impl MaskU12 for u16 {
    fn mask(self) -> u16 {
        self & u12::MAX.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_add_as_expected() {
        assert_eq!(u12::new(12), u12::new(5) + u12::new(7))
    }

    #[test]
    #[should_panic]
    #[allow(unused_must_use)]
    #[allow(clippy::unnecessary_operation)]
    fn should_panic_on_overflowing_add() {
        u12::new(0xFFFF) + u12::new(1);
    }

    #[test]
    fn should_wrap_correctly_with_overflowing_wrapping_add() {
        // non-overflowing add.
        assert_eq!(u12::new(12), (u12::new(5).wrapping_add(u12::new(7))));

        // overflowing add.
        assert_eq!(u12::new(1), (u12::new(0xFFF).wrapping_add(u12::new(2))))
    }

    #[test]
    fn should_wrap_correctly_with_overflowing_wrapping_sub() {
        // non-overflowing add.
        assert_eq!(u12::new(2), (u12::new(7).wrapping_sub(u12::new(5))));

        // overflowing add.
        assert_eq!(u12::new(0xFFF), (u12::new(0).wrapping_sub(u12::new(1))))
    }

    #[test]
    fn should_subtract_as_expected() {
        assert_eq!(u12::new(2), u12::new(7) - u12::new(5))
    }
}
