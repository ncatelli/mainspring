use crate::cpu::chip8::register;
use crate::cpu::chip8::u12::u12;
use crate::cpu::chip8::{microcode::*, Chip8, GenerateRandom};
use crate::cpu::Generate;
use crate::prelude::v1::Register;
use parcel::prelude::v1::*;

pub mod addressing_mode;

/// Represents a mask to binary and against a u8 to return the upper nibble.
const UPPER_NIBBLE_MASK: u8 = 0xf0;

/// Represents a mask to binary and against a u8 to return the lower nibble.
const LOWER_NIBBLE_MASK: u8 = 0x0f;

#[cfg(test)]
mod tests;

/// ToNibble provides methods for fetching the upper and lower nibble of a byte.
pub trait ToNibble {
    fn to_upper_nibble(&self) -> u8;
    fn to_lower_nibble(&self) -> u8;
}

impl ToNibble for u8 {
    fn to_upper_nibble(&self) -> u8 {
        (self & UPPER_NIBBLE_MASK) >> 4
    }

    fn to_lower_nibble(&self) -> u8 {
        self & LOWER_NIBBLE_MASK
    }
}

/// ToNibbles defines a trait for converting a type from a value into its
/// corresponding nibbles.
pub trait ToNibbleBytes {
    fn to_be_nibbles(&self) -> [u8; 2];
    fn to_le_nibbles(&self) -> [u8; 2];
}

impl ToNibbleBytes for u8 {
    fn to_be_nibbles(&self) -> [u8; 2] {
        [self.to_upper_nibble(), self.to_lower_nibble()]
    }

    fn to_le_nibbles(&self) -> [u8; 2] {
        [self.to_lower_nibble(), self.to_upper_nibble()]
    }
}

pub fn matches_first_nibble_without_taking_input<'a>(
    opcode: u8,
) -> impl Parser<'a, &'a [(usize, u8)], u8> {
    move |input: &'a [(usize, u8)]| match input.get(0) {
        Some(&(pos, next)) if ((next & 0xf0) >> 4) == opcode => Ok(MatchStatus::Match {
            span: pos..pos + 1,
            remainder: &input[0..],
            inner: opcode,
        }),
        _ => Ok(MatchStatus::NoMatch(input)),
    }
}

/// Provides a Parser type for the OpcodeVariant enum. Constructing an
/// OpcodeVariant from a stream of bytes.
pub struct OpcodeVariantParser;

impl<'a, R> Parser<'a, &'a [(usize, u8)], Box<dyn Generate<Chip8<R>, Vec<Microcode>>>>
    for OpcodeVariantParser
where
    R: 'static,
{
    #[allow(clippy::clippy::type_complexity)]
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Box<dyn Generate<Chip8<R>, Vec<Microcode>>>> {
        parcel::one_of(vec![
            <Jp<NonV0Indexed, addressing_mode::Absolute>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Jp<V0Indexed, addressing_mode::Absolute>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Ld<addressing_mode::Absolute>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Ld<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Ld<addressing_mode::SoundTimerDestTx>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Ld<addressing_mode::DelayTimerDestTx>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Ld<addressing_mode::DelayTimerSrcTx>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Ld<addressing_mode::VxIIndirect>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Add<addressing_mode::Immediate>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Add<addressing_mode::IRegisterIndexed>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <And<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Or<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Xor<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
        ])
        .parse(input)
    }
}

/// Clear the display.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Cls {
    addressing_mode: addressing_mode::Implied,
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Cls> for Cls {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Cls> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xe0])
            .map(|_| Cls::default())
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
pub struct Ret {
    addressing_mode: addressing_mode::Implied,
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ret> for Ret {
    fn parse(&self, input: &'a [(usize, u8)]) -> parcel::ParseResult<&'a [(usize, u8)], Ret> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xee])
            .map(|_| Ret::default())
            .parse(input)
    }
}

impl From<Ret> for u16 {
    fn from(_: Ret) -> Self {
        0x00ee
    }
}

/// Reprents a Jp command that is v0 indexed.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct V0Indexed;

/// Reprents an absolute Jp command that is not v0 indexed.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct NonV0Indexed;

/// Jp the associated value to the value of the specified register. Setting
/// the register to the sum.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Jp<T, A> {
    r#type: std::marker::PhantomData<T>,
    pub addressing_mode: A,
}

impl<T, A> Jp<T, A> {
    pub fn new(addressing_mode: A) -> Self {
        Self {
            r#type: std::marker::PhantomData,
            addressing_mode,
        }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Jp<NonV0Indexed, addressing_mode::Absolute>>
    for Jp<NonV0Indexed, addressing_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Jp<NonV0Indexed, addressing_mode::Absolute>> {
        matches_first_nibble_without_taking_input(0x1)
            .and_then(|_| addressing_mode::Absolute::default())
            .map(Jp::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Jp<NonV0Indexed, addressing_mode::Absolute> {
    fn generate(&self, _: &Chip8<R>) -> Vec<Microcode> {
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            u16::from(self.addressing_mode.addr()).wrapping_sub(2),
        ))]
    }
}

// Jp Absolute + V0
impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Jp<V0Indexed, addressing_mode::Absolute>>
    for Jp<V0Indexed, addressing_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Jp<V0Indexed, addressing_mode::Absolute>> {
        matches_first_nibble_without_taking_input(0xb)
            .and_then(|_| addressing_mode::Absolute::default())
            .map(Jp::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Jp<V0Indexed, addressing_mode::Absolute> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let v0_val = cpu.read_gp_register(register::GpRegisters::V0);
        let abs_addr = self.addressing_mode.addr();
        let jmp_addr = abs_addr.wrapping_add(u12::new(v0_val as u16));

        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::ProgramCounter,
            u16::from(jmp_addr).wrapping_sub(2),
        ))]
    }
}

/// Load the absolute value specified into the I register
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Ld<A> {
    pub addressing_mode: A,
}

impl<A> Ld<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ld<addressing_mode::Absolute>>
    for Ld<addressing_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ld<addressing_mode::Absolute>> {
        matches_first_nibble_without_taking_input(0xA)
            .and_then(|_| addressing_mode::Absolute::default())
            .map(Ld::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ld<addressing_mode::Absolute> {
    fn generate(&self, _: &Chip8<R>) -> Vec<Microcode> {
        vec![Microcode::Write16bitRegister(Write16bitRegister::new(
            register::WordRegisters::I,
            u16::from(self.addressing_mode.addr()),
        ))]
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ld<addressing_mode::Immediate>>
    for Ld<addressing_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ld<addressing_mode::Immediate>> {
        matches_first_nibble_without_taking_input(0x6)
            .and_then(|_| addressing_mode::Immediate::default())
            .map(Ld::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ld<addressing_mode::Immediate> {
    fn generate(&self, _: &Chip8<R>) -> Vec<Microcode> {
        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.register),
            self.addressing_mode.value,
        ))]
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ld<addressing_mode::VxVy>>
    for Ld<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ld<addressing_mode::VxVy>> {
        matches_first_nibble_without_taking_input(0x8)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|v| v.to_lower_nibble() == 0x0)
                }),
            )
            .and_then(|_| addressing_mode::VxVy::default())
            .map(Ld::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ld<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.first);

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.second),
            src_val,
        ))]
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ld<addressing_mode::SoundTimerDestTx>>
    for Ld<addressing_mode::SoundTimerDestTx>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ld<addressing_mode::SoundTimerDestTx>> {
        matches_first_nibble_without_taking_input(0xF)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|&second| second == 0x18)
                }),
            )
            .and_then(|_| addressing_mode::SoundTimerDestTx::default())
            .map(Ld::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ld<addressing_mode::SoundTimerDestTx> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.src);

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::TimerRegisters(register::TimerRegisters::Sound),
            src_val,
        ))]
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ld<addressing_mode::DelayTimerDestTx>>
    for Ld<addressing_mode::DelayTimerDestTx>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ld<addressing_mode::DelayTimerDestTx>> {
        matches_first_nibble_without_taking_input(0xF)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|&second| second == 0x15)
                }),
            )
            .and_then(|_| addressing_mode::DelayTimerDestTx::default())
            .map(Ld::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ld<addressing_mode::DelayTimerDestTx> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.src);

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::TimerRegisters(register::TimerRegisters::Delay),
            src_val,
        ))]
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ld<addressing_mode::DelayTimerSrcTx>>
    for Ld<addressing_mode::DelayTimerSrcTx>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ld<addressing_mode::DelayTimerSrcTx>> {
        matches_first_nibble_without_taking_input(0xF)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|&second| second == 0x07)
                }),
            )
            .and_then(|_| addressing_mode::DelayTimerSrcTx::default())
            .map(Ld::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ld<addressing_mode::DelayTimerSrcTx> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = crate::cpu::register::Register::read(&cpu.dt);

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.dest),
            src_val,
        ))]
    }
}

const fn extract_hundreds_place(x: u8) -> u8 {
    x / 100
}

const fn extract_tens_place(x: u8) -> u8 {
    (x % 100) / 10
}

const fn extract_ones_place(x: u8) -> u8 {
    (x % 100) % 10
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ld<addressing_mode::VxIIndirect>>
    for Ld<addressing_mode::VxIIndirect>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ld<addressing_mode::VxIIndirect>> {
        matches_first_nibble_without_taking_input(0xF)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|&second| second == 0x18)
                }),
            )
            .and_then(|_| addressing_mode::VxIIndirect::default())
            .map(Ld::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ld<addressing_mode::VxIIndirect> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.src);
        let hundreds = extract_hundreds_place(src_val);
        let tens = extract_tens_place(src_val);
        let ones = extract_ones_place(src_val);

        vec![
            Microcode::WriteMemory(WriteMemory::new(cpu.i.read(), hundreds)),
            Microcode::WriteMemory(WriteMemory::new(cpu.i.read() + 1, tens)),
            Microcode::WriteMemory(WriteMemory::new(cpu.i.read() + 2, ones)),
        ]
    }
}

/// Call subroutine at nnn.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Call<A> {
    pub addressing_mode: A,
}

impl<A> Call<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Call<addressing_mode::Absolute>>
    for Call<addressing_mode::Absolute>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Call<addressing_mode::Absolute>> {
        matches_first_nibble_without_taking_input(0x2)
            .and_then(|_| addressing_mode::Absolute::default())
            .map(Call::new)
            .parse(input)
    }
}

/// Adds the associated value to the value of the specified register. Setting
/// the register to the sum.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Add<A> {
    pub addressing_mode: A,
}

impl<A> Add<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Add<addressing_mode::Immediate>>
    for Add<addressing_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Add<addressing_mode::Immediate>> {
        matches_first_nibble_without_taking_input(0x7)
            .and_then(|_| addressing_mode::Immediate::default())
            .map(Add::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Add<addressing_mode::Immediate> {
    fn generate(&self, _: &Chip8<R>) -> Vec<Microcode> {
        vec![Microcode::Inc8bitRegister(Inc8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.register),
            self.addressing_mode.value,
        ))]
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Add<addressing_mode::IRegisterIndexed>>
    for Add<addressing_mode::IRegisterIndexed>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Add<addressing_mode::IRegisterIndexed>> {
        matches_first_nibble_without_taking_input(0xf)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte()
                    .and_then(|_| parcel::parsers::byte::expect_byte(0x1e)),
            )
            .and_then(|_| addressing_mode::IRegisterIndexed::default())
            .map(Add::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Add<addressing_mode::IRegisterIndexed> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let gp_val = cpu.read_gp_register(self.addressing_mode.register);
        vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
            register::WordRegisters::I,
            gp_val as u16,
        ))]
    }
}

/// And represents a binary & operation.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct And<A> {
    pub addressing_mode: A,
}

impl<A> And<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], And<addressing_mode::VxVy>>
    for And<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], And<addressing_mode::VxVy>> {
        matches_first_nibble_without_taking_input(0x8)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|&v| v.to_lower_nibble() == 0x02)
                }),
            )
            .and_then(|_| addressing_mode::VxVy::default())
            .map(And::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for And<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.first);
        let dest_val = cpu.read_gp_register(self.addressing_mode.second);
        let result = dest_val & src_val;

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.second),
            result,
        ))]
    }
}

/// Or represents a binary | operation.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Or<A> {
    pub addressing_mode: A,
}

impl<A> Or<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Or<addressing_mode::VxVy>>
    for Or<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Or<addressing_mode::VxVy>> {
        matches_first_nibble_without_taking_input(0x8)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|v| v.to_lower_nibble() == 0x01)
                }),
            )
            .and_then(|_| addressing_mode::VxVy::default())
            .map(Or::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Or<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.first);
        let dest_val = cpu.read_gp_register(self.addressing_mode.second);
        let result = dest_val | src_val;

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.second),
            result,
        ))]
    }
}

/// Xor represents a binary ^ operation.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Xor<A> {
    pub addressing_mode: A,
}

impl<A> Xor<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Xor<addressing_mode::VxVy>>
    for Xor<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Xor<addressing_mode::VxVy>> {
        matches_first_nibble_without_taking_input(0x8)
            .peek_next(
                // discard the first byte since the previous parser takes nothing.
                parcel::parsers::byte::any_byte().and_then(|_| {
                    parcel::parsers::byte::any_byte().predicate(|v| v.to_lower_nibble() == 0x03)
                }),
            )
            .and_then(|_| addressing_mode::VxVy::default())
            .map(Xor::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Xor<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.first);
        let dest_val = cpu.read_gp_register(self.addressing_mode.second);
        let result = dest_val ^ src_val;

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.second),
            result,
        ))]
    }
}

/// Se skips the next instruction if the operands are equivalent.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Se<A> {
    pub addressing_mode: A,
}

impl<A> Se<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Se<addressing_mode::VxVy>>
    for Se<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Se<addressing_mode::VxVy>> {
        matches_first_nibble_without_taking_input(0x5)
            .peek_next(parcel::parsers::byte::any_byte().and_then(|_| {
                parcel::parsers::byte::any_byte().predicate(|v| v.to_lower_nibble() == 0x00)
            }))
            .and_then(|_| addressing_mode::VxVy::default())
            .map(Se::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Se<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let first_reg_val = cpu.read_gp_register(self.addressing_mode.first);
        let second_reg_val = cpu.read_gp_register(self.addressing_mode.second);

        if first_reg_val == second_reg_val {
            vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                2,
            ))]
        } else {
            vec![]
        }
    }
}

/// Rnd generates a random 8-bit value to be applied against a mask.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Rnd<A> {
    pub addressing_mode: A,
}

impl<A> Rnd<A> {
    pub fn new(addressing_mode: A) -> Self {
        Rnd { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Rnd<addressing_mode::Immediate>>
    for Rnd<addressing_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Rnd<addressing_mode::Immediate>> {
        matches_first_nibble_without_taking_input(0xC)
            .and_then(|_| addressing_mode::Immediate::default())
            .map(Rnd::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Rnd<addressing_mode::Immediate>
where
    R: GenerateRandom<u8>,
{
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let rand = cpu.rng.random();
        let value = rand & self.addressing_mode.value;

        vec![Microcode::Write8bitRegister(Write8bitRegister::new(
            register::ByteRegisters::GpRegisters(self.addressing_mode.register),
            value,
        ))]
    }
}
