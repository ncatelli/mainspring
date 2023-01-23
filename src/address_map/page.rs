/// Page represents an 8-bit memory page for the purpose of determining if an
/// address falls within the space of a page.
pub struct Page<T>
where
    T: Ord,
{
    inner: std::ops::RangeInclusive<T>,
}

impl<T> Page<T>
where
    T: Ord,
{
    /// new takes a start and end value, generating an inclusive range representing the page.
    #[allow(unused)]
    pub fn new(start: T, end: T) -> Self {
        Self { inner: start..=end }
    }

    /// Returns true if the passed address falls within the range of the page.
    pub fn contains(&self, addr: T) -> bool {
        self.inner.contains(&addr)
    }
}

impl From<u8> for Page<u16> {
    // Conversion from a u8 always implies the the page is the zero page.
    fn from(_: u8) -> Self {
        Self { inner: 0x00..=0xff }
    }
}

impl From<u16> for Page<u16> {
    fn from(addr: u16) -> Self {
        let page_size = 0xff;
        let upper_page_bound: u16 = addr + (page_size - (addr % (page_size + 1)));
        let lower_page_bound: u16 = upper_page_bound - page_size;

        Self {
            inner: lower_page_bound..=upper_page_bound,
        }
    }
}
