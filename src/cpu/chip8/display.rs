const fn is_within_display_boundary(origin: (usize, usize)) -> bool {
    let x = origin.0;
    let y = origin.1;

    // check that x and y each have enough padding from their sprite origin.
    (x < Display::X_MAX) && (y < Display::Y_MAX)
}

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
    /// The maximum number of columns per row.
    const X_MAX: usize = 64;

    /// The maximum number of rows.
    const Y_MAX: usize = 32;

    /// Returns the maximum number of columns per row.
    pub fn x_max() -> usize {
        Self::X_MAX
    }

    /// Returns the maximum number of rows.
    pub fn y_max() -> usize {
        Self::Y_MAX
    }

    /// gets the value of the pixel specified by the cartesian coordinates `x`,
    /// `y`. If the coordinates are within range, an `Option::Some(bool)` with
    /// the value of the pixel is returned. Otherwise `Option::None` is
    /// returned.
    pub fn pixel(&self, x: usize, y: usize) -> Option<bool> {
        if is_within_display_boundary((x, y)) {
            Some(self.inner[y][x])
        } else {
            None
        }
    }

    /// Provides a function handler for making mutable changes to the inner display
    /// representation.
    pub fn with_inner_mut<F, B>(&mut self, f: F) -> B
    where
        F: Fn(&mut [[bool; 64]; 32]) -> B,
    {
        (f)(&mut self.inner)
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
        self.pixel(x, y).map(|previous_value| {
            // if we can get the previous value, it's safe to write a new one.
            self.inner[y][x] = pixel_on;
            previous_value
        })
    }

    /// Takes an origin x and y value along with a sprite and attempts to
    /// write the value to the display, returning an
    /// `(Self, Option::Some(true))` if successfully written.
    pub fn write_sprite(mut self, x: usize, y: usize, sprite: Font) -> (Self, Option<bool>) {
        let modified = self.write_sprite_mut(x, y, sprite);
        (self, modified)
    }

    /// Takes an origin x and y value along with a sprite and attempts to
    /// write the value to the display, returning an `Option::Some(true)` if
    /// successfully written.
    pub fn write_sprite_mut(&mut self, x: usize, y: usize, sprite: Font) -> Option<bool> {
        let font_bytes: [u8; 5] = sprite.into();

        // sprite starts in display boundary
        is_within_display_boundary((x, y)).then(|| {
            (0..8u8)
                .map(|x| (0..5usize).map(|y| (x, y)).collect::<Vec<_>>())
                .flatten()
                .fold(false, |collision, (x_offset, y_offset)| {
                    let bit = (font_bytes[y_offset] >> x_offset) & 0x1;
                    let bit_is_set = bit != 0;
                    let adjusted_y = y + y_offset;
                    let adjusted_x = x + x_offset as usize;

                    // if the pixel can't be written default to last collision
                    // value so state isn't lost.
                    self.write_pixel_mut(adjusted_x, adjusted_y, bit_is_set)
                        .unwrap_or(collision)
                })
        })
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
