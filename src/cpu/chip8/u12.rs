/// Represents an unsigned 12-bit integer.
#[derive(Clone, Copy)]
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
    /// Instantiates a new u12.
    pub fn new(value: u16) -> Self {
        if value > Self::MAX.0 {
            panic!("this value will overflow")
        } else {
            Self(value)
        }
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
        mask(*self).0 == mask(*other).0
    }
}

impl Eq for u12 {}

impl core::cmp::PartialOrd for u12 {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        mask(*self).0.partial_cmp(&mask(*other).0)
    }
}

impl core::cmp::Ord for u12 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        mask(*self).0.cmp(&mask(*other).0)
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
        mask(Self(lhs - rhs))
    }
}

impl core::ops::BitAnd for u12 {
    type Output = Self;

    fn bitand(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.0, other.0);
        mask(Self(lhs & rhs))
    }
}

impl core::ops::BitOr for u12 {
    type Output = Self;

    fn bitor(self, other: Self) -> Self::Output {
        let (lhs, rhs) = (self.0, other.0);
        mask(Self(lhs | rhs))
    }
}

impl core::ops::Shl for u12 {
    type Output = Self;

    fn shl(self, other: Self) -> Self {
        let (lhs, rhs) = (self.0, other.0);
        mask(Self(lhs << rhs))
    }
}

impl core::ops::Shr for u12 {
    type Output = Self;

    fn shr(self, other: Self) -> Self {
        let (lhs, rhs) = (self.0, other.0);
        mask(Self(lhs >> rhs))
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

fn mask(value: u12) -> u12 {
    u12::new(value.0 & u12::MAX.0)
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
    fn should_panic_on_overflowing_add() {
        u12::new(0xFFFF) + u12::new(1);
    }

    #[test]
    fn should_subtract_as_expected() {
        assert_eq!(u12::new(2), u12::new(7) - u12::new(5))
    }
}
