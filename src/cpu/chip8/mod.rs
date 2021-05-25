mod register;

/// Chip8 represents a CHIP-8 CPU.
#[derive(Debug, Clone)]
pub struct Chip8 {
    address_space: [u8; 0xfff],
    v0: register::GeneralPurpose,
    v1: register::GeneralPurpose,
    v2: register::GeneralPurpose,
    v3: register::GeneralPurpose,
    v4: register::GeneralPurpose,
    v5: register::GeneralPurpose,
    v6: register::GeneralPurpose,
    v7: register::GeneralPurpose,
    v8: register::GeneralPurpose,
    v9: register::GeneralPurpose,
    va: register::GeneralPurpose,
    vb: register::GeneralPurpose,
    vc: register::GeneralPurpose,
    vd: register::GeneralPurpose,
    ve: register::GeneralPurpose,
    vf: register::GeneralPurpose,
}
