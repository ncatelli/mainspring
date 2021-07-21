/// Display mimics the display matrix for the CHIP-8 isa.
#[derive(Debug, Clone, Copy)]
pub struct Display {
    inner: [[bool; 64]; 32],
}

impl Default for Display {
    fn default() -> Self {
        Self {
            inner: [[false; 64]; 32],
        }
    }
}

impl Display {
    /// Returns the maximum number of columns per row.
    pub fn x_max() -> usize {
        64
    }

    /// Returns the maximum number of rows.
    pub fn y_max() -> usize {
        32
    }

    /// gets the value of the pixel specified by the cartesian coordinates `x`,
    /// `y`. If the coordinates are within range, an `Option::Some(bool)` with
    /// the value of the pixel is returned. Otherwise `Option::None` is
    /// returned.
    pub fn pixel(&self, x: usize, y: usize) -> Option<bool> {
        if x < Self::x_max() && y < Self::y_max() {
            Some(self.inner[y][x])
        } else {
            None
        }
    }

    /// sets the value of the pixel specified by the cartesian coordinates `x`,
    /// `y` to the boolean value specified by `pixel_on`. If the coordinates
    /// are within range, a `(Self, Option::Some(bool))` with the previous
    /// value of the pixel is returned. Otherwise `(Self, Option::None)` is
    /// returned.
    pub fn write_pixel(mut self, x: usize, y: usize, pixel_on: bool) -> (Self, Option<bool>) {
        let modified = self.write_pixel_mut(x, y, pixel_on);
        (self, modified)
    }

    /// sets the value of the pixel specified by the cartesian coordinates `x`,
    /// `y` to the boolean value specified by `pixel_on`. If the coordinates
    /// are within range, an `Option::Some(bool)` with the previous value of
    /// the pixel is returned. Otherwise `Option::None` is returned.
    pub fn write_pixel_mut(&mut self, x: usize, y: usize, pixel_on: bool) -> Option<bool> {
        if let Some(previous_value) = self.pixel(x, y) {
            // if we can get the previous value, it's safe to write a new one.
            self.inner[y][x] = pixel_on;
            Some(previous_value)
        } else {
            None
        }
    }

    /// Returns the enclosed multi-dimensional array representing the display
    /// pixels, consuming the original Display in the process.
    pub fn unwrap(self) -> [[bool; 64]; 32] {
        self.inner
    }
}

/// Represents each valid Sprite in the default font.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Font {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Zero,
    A,
    B,
    C,
    D,
    E,
    F,
}

impl From<Font> for [u8; 5] {
    fn from(font: Font) -> Self {
        match font {
            Font::One => [0x20, 0x60, 0x20, 0x20, 0x70],
            Font::Two => [0xF0, 0x10, 0xF0, 0x80, 0xF0],
            Font::Three => [0xF0, 0x10, 0xF0, 0x10, 0xF0],
            Font::Four => [0x90, 0x90, 0xF0, 0x10, 0x10],
            Font::Five => [0xF0, 0x80, 0xF0, 0x10, 0xF0],
            Font::Six => [0xF0, 0x80, 0xF0, 0x90, 0xF0],
            Font::Seven => [0xF0, 0x10, 0x20, 0x40, 0x40],
            Font::Eight => [0xF0, 0x90, 0xF0, 0x90, 0xF0],
            Font::Nine => [0xF0, 0x90, 0xF0, 0x10, 0xF0],
            Font::Zero => [0xF0, 0x90, 0x90, 0x90, 0xF0],
            Font::A => [0xF0, 0x90, 0xF0, 0x90, 0x90],
            Font::B => [0xE0, 0x90, 0xE0, 0x90, 0xE0],
            Font::C => [0xF0, 0x80, 0x80, 0x80, 0xF0],
            Font::D => [0xE0, 0x90, 0x90, 0x90, 0xE0],
            Font::E => [0xF0, 0x80, 0xF0, 0x80, 0xF0],
            Font::F => [0xF0, 0x80, 0xF0, 0x80, 0x80],
        }
    }
}

impl From<Font> for Vec<u8> {
    fn from(font: Font) -> Self {
        let array: [u8; 5] = font.into();
        array.to_vec()
    }
}
