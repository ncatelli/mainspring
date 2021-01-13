pub trait Register<R, W> {
    fn read(&self) -> R;
    fn write(self, value: W) -> Self;
    fn with_value(value: W) -> Self;
}
