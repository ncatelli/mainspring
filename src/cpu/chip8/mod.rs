use crate::address_map::AddressMap;
mod register;

/// Chip8 represents a CHIP-8 CPU.
#[derive(Debug, Clone)]
pub struct Chip8 {
    address_space: AddressMap<u16>,
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

impl Chip8 {
    pub fn with_register(
        mut self,
        reg_type: register::GPRegisters,
        reg: register::GeneralPurpose,
    ) -> Self {
        match reg_type {
            register::GPRegisters::V0 => self.v0 = reg,
            register::GPRegisters::V1 => self.v1 = reg,
            register::GPRegisters::V2 => self.v2 = reg,
            register::GPRegisters::V3 => self.v3 = reg,
            register::GPRegisters::V4 => self.v4 = reg,
            register::GPRegisters::V5 => self.v5 = reg,
            register::GPRegisters::V6 => self.v6 = reg,
            register::GPRegisters::V7 => self.v7 = reg,
            register::GPRegisters::V8 => self.v8 = reg,
            register::GPRegisters::V9 => self.v9 = reg,
            register::GPRegisters::VA => self.va = reg,
            register::GPRegisters::VB => self.vb = reg,
            register::GPRegisters::VC => self.vc = reg,
            register::GPRegisters::VD => self.vd = reg,
            register::GPRegisters::VE => self.ve = reg,
            register::GPRegisters::VF => self.vf = reg,
        };
        self
    }
}

impl Default for Chip8 {
    fn default() -> Self {
        Self {
            address_space: AddressMap::default(),
            v0: register::GeneralPurpose::default(),
            v1: register::GeneralPurpose::default(),
            v2: register::GeneralPurpose::default(),
            v3: register::GeneralPurpose::default(),
            v4: register::GeneralPurpose::default(),
            v5: register::GeneralPurpose::default(),
            v6: register::GeneralPurpose::default(),
            v7: register::GeneralPurpose::default(),
            v8: register::GeneralPurpose::default(),
            v9: register::GeneralPurpose::default(),
            va: register::GeneralPurpose::default(),
            vb: register::GeneralPurpose::default(),
            vc: register::GeneralPurpose::default(),
            vd: register::GeneralPurpose::default(),
            ve: register::GeneralPurpose::default(),
            vf: register::GeneralPurpose::default(),
        }
    }
}
