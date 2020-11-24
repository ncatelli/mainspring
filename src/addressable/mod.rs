pub struct StaticAddressable<T, U> {
    address_space: T,
    start_address: T,
    stop_address: T,
    buffer: U,
}
