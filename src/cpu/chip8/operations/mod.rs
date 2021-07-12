use crate::address_map::SafeAddressable;
use crate::cpu::chip8::register;
use crate::cpu::chip8::register::GpRegisters;
use crate::cpu::chip8::u12::u12;
use crate::cpu::chip8::{microcode::*, Chip8, GenerateRandom};
use crate::cpu::Generate;
use crate::prelude::v1::Register;
use parcel::prelude::v1::*;

pub mod addressing_mode;

/// A placeholder constant error string until a u4 type is implemented. Other
/// assertions are in place so that this should never be encountered.
const NIBBLE_OVERFLOW: &str = "unreachable nibble should be limited to u4.";

/// Represents a mask to binary and against a u8 to return the upper nibble.
const UPPER_NIBBLE_MASK: u8 = 0xf0;

/// Represents a mask to binary and against a u8 to return the lower nibble.
const LOWER_NIBBLE_MASK: u8 = 0x0f;

/// Returns a u8 representing the input byte with the most significant
/// masked limiting the maximum value to 0x0f.
const fn least_significant_nibble_from_u8(x: u8) -> u8 {
    x & LOWER_NIBBLE_MASK
}

/// Generates a u8 from two nibbles. This expectes both input values to
/// respect the maximum value range of a nibble as the most significant bits
/// are left shifted to accommodate the least significant bits.
const fn u8_from_nibbles(msb: u8, lsb: u8) -> u8 {
    let masked_lsb = least_significant_nibble_from_u8(lsb);
    (msb << 4) | masked_lsb
}

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

/// This function takes an array of four optional u8 values representing
/// nibbles of an instruction. If the `Option` is set to `None`, any value will
/// match for that nibble. If the `Option` is `Some`, the enclosed value will
/// need to match the corresponding nibble of associated instruction.
/// i.e. `[Some(0xf), None, None, None]` would match `[0xf, 0x1, 0x2, 0x3]`.
fn expect_instruction_with_mask<'a>(
    expected: [Option<u8>; 4],
) -> impl Parser<'a, &'a [(usize, u8)], [u8; 4]> {
    move |input: &'a [(usize, u8)]| {
        parcel::take_n(parcel::parsers::byte::any_byte(), 2)
            .map(|bytes| [bytes[0].to_be_nibbles(), bytes[1].to_be_nibbles()])
            .map(|[[first, second], [third, fourth]]| [first, second, third, fourth])
            .predicate(move |nibbles| instruction_matches_nibble_mask(*nibbles, expected).is_ok())
            .parse(input)
    }
}

fn nibble_matches_mask(input: u8, expected: Option<u8>) -> bool {
    match expected {
        // return true if any value matches.
        None => true,
        // return true if the exected value matches input.
        Some(e) if e == input => true,
        // return false in all other cases
        Some(_) => false,
    }
}

fn instruction_matches_nibble_mask(
    input: [u8; 4],
    expected: [Option<u8>; 4],
) -> Result<[u8; 4], String> {
    let nibble_matches = input
        .iter()
        .zip(expected.iter())
        .map(|(&i, &e)| nibble_matches_mask(i, e))
        .take_while(|v| *v)
        .count();

    // validate that all nibbles passed their mask
    if nibble_matches == 4 {
        Ok(input)
    } else {
        Err(format!(
            "failed to match nibble at position: {}. got: {}, wanted: {:?}",
            nibble_matches, input[nibble_matches], expected[nibble_matches]
        ))
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
    #[allow(clippy::type_complexity)]
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Box<dyn Generate<Chip8<R>, Vec<Microcode>>>> {
        parcel::one_of(vec![
            <Ret<addressing_mode::Implied>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Call<addressing_mode::Absolute>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
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
            <Add<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Subn<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <And<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Or<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Xor<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Se<addressing_mode::VxVy>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
            <Se<addressing_mode::Immediate>>::default()
                .map(|opc| Box::new(opc) as Box<dyn Generate<Chip8<R>, Vec<Microcode>>>),
        ])
        .parse(input)
    }
}

/// Clear the display.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Cls<A> {
    addressing_mode: std::marker::PhantomData<A>,
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Cls<addressing_mode::Implied>>
    for Cls<addressing_mode::Implied>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Cls<addressing_mode::Implied>> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xe0])
            .map(|_| Cls::default())
            .parse(input)
    }
}

impl From<Cls<addressing_mode::Implied>> for u16 {
    fn from(_: Cls<addressing_mode::Implied>) -> Self {
        0x00e0
    }
}

/// Return from a subroutine.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Ret<A> {
    addressing_mode: std::marker::PhantomData<A>,
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Ret<addressing_mode::Implied>>
    for Ret<addressing_mode::Implied>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Ret<addressing_mode::Implied>> {
        parcel::parsers::byte::expect_bytes(&[0x00, 0xee])
            .map(|_| Ret::default())
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Ret<addressing_mode::Implied> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let current_sp = cpu.sp.read();
        let ret_pc = cpu.stack.read(current_sp as usize);
        let inc_adjusted_addr = u16::from(ret_pc).wrapping_sub(2);

        vec![
            Microcode::PopStack(PopStack::new(ret_pc)),
            Microcode::Write16bitRegister(Write16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                inc_adjusted_addr,
            )),
        ]
    }
}

impl From<Ret<addressing_mode::Implied>> for u16 {
    fn from(_: Ret<addressing_mode::Implied>) -> Self {
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
        expect_instruction_with_mask([Some(0x1), None, None, None])
            .map(|[_, first, second, third]| u12::from_be_nibbles([first, second, third]))
            .map(addressing_mode::Absolute::new)
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
        expect_instruction_with_mask([Some(0xB), None, None, None])
            .map(|[_, first, second, third]| u12::from_be_nibbles([first, second, third]))
            .map(addressing_mode::Absolute::new)
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
        expect_instruction_with_mask([Some(0xA), None, None, None])
            .map(|[_, first, second, third]| u12::from_be_nibbles([first, second, third]))
            .map(addressing_mode::Absolute::new)
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
        expect_instruction_with_mask([Some(0x6), None, None, None])
            .map(|[_, dest, msb, lsb]| {
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (dest_reg, u8_from_nibbles(msb, lsb))
            })
            .map(|(dest, value)| addressing_mode::Immediate::new(dest, value))
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
        expect_instruction_with_mask([Some(0x8), None, None, Some(0x0)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
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
        expect_instruction_with_mask([Some(0xF), None, Some(0x1), Some(0x8)])
            .map(|[_, reg_id, _, _]| {
                std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW)
            })
            .map(addressing_mode::SoundTimerDestTx::new)
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
        expect_instruction_with_mask([Some(0xF), None, Some(0x1), Some(0x5)])
            .map(|[_, reg_id, _, _]| {
                std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW)
            })
            .map(addressing_mode::DelayTimerDestTx::new)
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
        expect_instruction_with_mask([Some(0xF), None, Some(0x0), Some(0x7)])
            .map(|[_, reg_id, _, _]| {
                std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW)
            })
            .map(addressing_mode::DelayTimerSrcTx::new)
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
        expect_instruction_with_mask([Some(0xF), None, Some(0x1), Some(0x8)])
            .map(|[_, reg_id, _, _]| {
                std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW)
            })
            .map(addressing_mode::VxIIndirect::new)
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
        expect_instruction_with_mask([Some(0x2), None, None, None])
            .map(|[_, first, second, third]| u12::from_be_nibbles([first, second, third]))
            .map(addressing_mode::Absolute::new)
            .map(Call::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Call<addressing_mode::Absolute> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let current_pc = cpu.pc.read();
        let addr = self.addressing_mode.addr();
        // decrement 2 to account for PC incrementing.
        let inc_adjusted_addr = u16::from(addr).wrapping_sub(2);

        vec![
            Microcode::PushStack(PushStack::new(current_pc)),
            Microcode::Write16bitRegister(Write16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                inc_adjusted_addr,
            )),
        ]
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
        expect_instruction_with_mask([Some(0x7), None, None, None])
            .map(|[_, dest, msb, lsb]| {
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (dest_reg, u8_from_nibbles(msb, lsb))
            })
            .map(|(dest, value)| addressing_mode::Immediate::new(dest, value))
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
        expect_instruction_with_mask([Some(0xf), None, Some(0x1), Some(0xe)])
            .map(|[_, reg_id, _, _]| {
                std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW)
            })
            .map(addressing_mode::IRegisterIndexed::new)
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

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Add<addressing_mode::VxVy>>
    for Add<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Add<addressing_mode::VxVy>> {
        expect_instruction_with_mask([Some(0x8), None, None, Some(0x4)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
            .map(Add::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Add<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.first);
        let dest_val = cpu.read_gp_register(self.addressing_mode.second);
        let (result, overflows) = dest_val.overflowing_add(src_val);
        let flag_val = if overflows { 1u8 } else { 0u8 };

        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(self.addressing_mode.second),
                result,
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                flag_val,
            )),
        ]
    }
}

/// Subtracts the associated value from the value of the specified register.
/// Setting the register to the difference. Sets the borrow flag if the
/// difference does not underflow
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Sub<A> {
    pub addressing_mode: A,
}

impl<A> Sub<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Sub<addressing_mode::VxVy>>
    for Sub<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Sub<addressing_mode::VxVy>> {
        expect_instruction_with_mask([Some(0x8), None, None, Some(0x5)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
            .map(Sub::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Sub<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.first);
        let dest_val = cpu.read_gp_register(self.addressing_mode.second);
        let (result, underflows) = dest_val.overflowing_sub(src_val);
        let flag_val = if underflows { 0u8 } else { 1u8 };

        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(self.addressing_mode.second),
                result,
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                flag_val,
            )),
        ]
    }
}

/// Subtracts the associated value from the value of the specified register.
/// Setting the register to the difference. Sets the borrow flag if the
/// difference does not underflow
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Subn<A> {
    pub addressing_mode: A,
}

impl<A> Subn<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Subn<addressing_mode::VxVy>>
    for Subn<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Subn<addressing_mode::VxVy>> {
        expect_instruction_with_mask([Some(0x8), None, None, Some(0x7)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
            .map(Subn::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Subn<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let src_val = cpu.read_gp_register(self.addressing_mode.first);
        let dest_val = cpu.read_gp_register(self.addressing_mode.second);
        let (result, underflows) = src_val.overflowing_sub(dest_val);
        let flag_val = if underflows { 0u8 } else { 1u8 };

        vec![
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(self.addressing_mode.second),
                result,
            )),
            Microcode::Write8bitRegister(Write8bitRegister::new(
                register::ByteRegisters::GpRegisters(GpRegisters::Vf),
                flag_val,
            )),
        ]
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
        expect_instruction_with_mask([Some(0x8), None, None, Some(0x2)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
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
        expect_instruction_with_mask([Some(0x8), None, None, Some(0x1)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
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
        expect_instruction_with_mask([Some(0x8), None, None, Some(0x3)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
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

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Se<addressing_mode::Immediate>>
    for Se<addressing_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Se<addressing_mode::Immediate>> {
        expect_instruction_with_mask([Some(0x3), None, None, None])
            .map(|[_, reg_id, msb, lsb]| {
                let reg = std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW);
                (reg, u8_from_nibbles(msb, lsb))
            })
            .map(|(reg, value)| addressing_mode::Immediate::new(reg, value))
            .map(Se::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Se<addressing_mode::Immediate> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let reg_val = cpu.read_gp_register(self.addressing_mode.register);
        let value = self.addressing_mode.value;

        if reg_val == value {
            vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                2,
            ))]
        } else {
            vec![]
        }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Se<addressing_mode::VxVy>>
    for Se<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Se<addressing_mode::VxVy>> {
        expect_instruction_with_mask([Some(0x5), None, None, Some(0x0)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
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

/// Sne skips the next instruction if the operands are not equivalent.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Sne<A> {
    pub addressing_mode: A,
}

impl<A> Sne<A> {
    pub fn new(addressing_mode: A) -> Self {
        Self { addressing_mode }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Sne<addressing_mode::Immediate>>
    for Sne<addressing_mode::Immediate>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Sne<addressing_mode::Immediate>> {
        expect_instruction_with_mask([Some(0x4), None, None, None])
            .map(|[_, reg_id, msb, lsb]| {
                let reg = std::convert::TryFrom::<u8>::try_from(reg_id).expect(NIBBLE_OVERFLOW);
                (reg, u8_from_nibbles(msb, lsb))
            })
            .map(|(reg, value)| addressing_mode::Immediate::new(reg, value))
            .map(Sne::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Sne<addressing_mode::Immediate> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let reg_val = cpu.read_gp_register(self.addressing_mode.register);
        let value = self.addressing_mode.value;

        if reg_val != value {
            vec![Microcode::Inc16bitRegister(Inc16bitRegister::new(
                register::WordRegisters::ProgramCounter,
                2,
            ))]
        } else {
            vec![]
        }
    }
}

impl<'a> parcel::Parser<'a, &'a [(usize, u8)], Sne<addressing_mode::VxVy>>
    for Sne<addressing_mode::VxVy>
{
    fn parse(
        &self,
        input: &'a [(usize, u8)],
    ) -> parcel::ParseResult<&'a [(usize, u8)], Sne<addressing_mode::VxVy>> {
        expect_instruction_with_mask([Some(0x9), None, None, Some(0x0)])
            .map(|[_, dest, src, _]| {
                let src_reg = std::convert::TryFrom::<u8>::try_from(src).expect(NIBBLE_OVERFLOW);
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (src_reg, dest_reg)
            })
            .map(|(src, dest)| addressing_mode::VxVy::new(src, dest))
            .map(Sne::new)
            .parse(input)
    }
}

impl<R> Generate<Chip8<R>, Vec<Microcode>> for Sne<addressing_mode::VxVy> {
    fn generate(&self, cpu: &Chip8<R>) -> Vec<Microcode> {
        let first_reg_val = cpu.read_gp_register(self.addressing_mode.first);
        let second_reg_val = cpu.read_gp_register(self.addressing_mode.second);

        if first_reg_val != second_reg_val {
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
        expect_instruction_with_mask([Some(0xC), None, None, None])
            .map(|[_, dest, msb, lsb]| {
                let dest_reg = std::convert::TryFrom::<u8>::try_from(dest).expect(NIBBLE_OVERFLOW);
                (dest_reg, u8_from_nibbles(msb, lsb))
            })
            .map(|(dest, value)| addressing_mode::Immediate::new(dest, value))
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
