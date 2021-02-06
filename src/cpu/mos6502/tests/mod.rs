use crate::address_map::{
    memory::{Memory, ReadOnly, ReadWrite},
    Addressable,
};
use crate::cpu::{
    mos6502::{register, register::GPRegister, MOS6502},
    register::Register,
    CPU,
};

fn generate_test_cpu_with_instructions(opcodes: Vec<u8>) -> MOS6502 {
    let (start_addr, stop_addr) = (0x6000, 0x7000);
    let mut nop_sled = [0xea; 0x7000 - 0x6000].to_vec();
    for (index, val) in opcodes.into_iter().enumerate() {
        nop_sled[index] = val;
    }

    MOS6502::default()
        .reset()
        .unwrap()
        .with_pc_register(register::ProgramCounter::with_value(start_addr))
        .register_address_space(
            start_addr..=stop_addr,
            Memory::<ReadOnly>::new(0x6000, 0x7000).load(nop_sled),
        )
        .unwrap()
}

#[test]
fn should_cycle_on_adc_absolute_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x6d, 0xff, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80));
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_absolute_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x6d, 0xff, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10));
    cpu.address_map.write(0x00ff, 0x50).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_adc_absolute_indexed_with_x_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x7d, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_absolute_indexed_with_x_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x7d, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x50).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_adc_absolute_indexed_with_y_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x79, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_absolute_indexed_with_y_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x79, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x50).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_adc_indirect_y_indexed_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x71, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xff).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_indirect_y_indexed_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x71, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x50).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_adc_immediate_operation_with_overflow() {
    let cpu = generate_test_cpu_with_instructions(vec![0x69, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_x_indexed_indirect_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x61, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xff).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_x_indexed_indirect_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x61, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x50).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_adc_immediate_operation_without_overflow() {
    let cpu = generate_test_cpu_with_instructions(vec![0x69, 0x50])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_adc_zeropage_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x65, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80));
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_zeropage_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x65, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10));
    cpu.address_map.write(0x00ff, 0x50).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_adc_zeropage_indexed_with_x_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x75, 0xfa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x80))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7f, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, true, false)
    );
}

#[test]
fn should_cycle_on_adc_zeropage_indexed_with_x_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x75, 0xfa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x10))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x50).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x60, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, false, false, false)
    );
}

#[test]
fn should_cycle_on_and_absolute_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x2d, 0xff, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_and_absolute_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x3d, 0xfa, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_and_absolute_indexed_with_y_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x39, 0xfa, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_and_indirect_y_indexed_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x31, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_and_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x29, 0x55])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_and_x_indexed_indirect_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x21, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_and_zeropage_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x25, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_and_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x35, 0xfa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x55, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn bcc_relative_operation_should_jump_when_zero_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x90, 0x08]);
    cpu.ps.carry = false;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn bcc_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x90, 0xf8]);
    cpu.ps.carry = false;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn bcc_relative_operation_should_not_jump_when_zero_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x90, 0x08]);
    cpu.ps.carry = true;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn bcs_relative_operation_should_jump_when_zero_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb0, 0x08]);
    cpu.ps.carry = true;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn bcs_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb0, 0xf8]);
    cpu.ps.carry = true;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn bcs_relative_operation_should_not_jump_when_zero_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb0, 0x08]);
    cpu.ps.carry = false;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn beq_relative_operation_should_jump_when_zero_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf0, 0x08]);
    cpu.ps.zero = true;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn beq_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf0, 0xf8]);
    cpu.ps.zero = true;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn beq_relative_operation_should_not_jump_when_zero_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf0, 0x08]);
    cpu.ps.zero = false;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn bmi_relative_operation_should_jump_when_zero_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x30, 0x08]);
    cpu.ps.negative = true;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn bmi_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x30, 0xf8]);
    cpu.ps.negative = true;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn bmi_relative_operation_should_not_jump_when_zero_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x30, 0x08]);
    cpu.ps.negative = false;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn bne_relative_operation_should_jump_when_zero_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd0, 0x08]);
    cpu.ps.zero = false;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn bne_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd0, 0xf8]);
    cpu.ps.zero = false;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn bne_relative_operation_should_not_jump_when_zero_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd0, 0x08]);
    cpu.ps.zero = true;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn bpl_relative_operation_should_jump_when_zero_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x10, 0x08]);
    cpu.ps.negative = false;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn bpl_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x10, 0xf8]);
    cpu.ps.negative = false;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn bpl_relative_operation_should_not_jump_when_zero_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x10, 0x08]);
    cpu.ps.negative = true;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn bvc_relative_operation_should_jump_when_overflow_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x50, 0x08]);
    cpu.ps.overflow = false;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn bvc_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x50, 0xf8]);
    cpu.ps.overflow = false;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn bvc_relative_operation_should_not_jump_when_overflow_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x50, 0x08]);
    cpu.ps.overflow = true;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn bvs_relative_operation_should_jump_when_overflow_set() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x70, 0x08]);
    cpu.ps.overflow = true;

    // 3 cycles with branch penalty
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6008, state.pc.read());
}

#[test]
fn bvs_relative_operation_should_incur_penalty_at_page_boundary() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x70, 0xf8]);
    cpu.ps.overflow = true;

    // 4 cycles with branch penalty
    let state = cpu.run(4).unwrap();
    assert_eq!(0x5ff8, state.pc.read());
}

#[test]
fn bvs_relative_operation_should_not_jump_when_overflow_unset() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x70, 0x08]);
    cpu.ps.overflow = false;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
}

#[test]
fn should_cycle_on_clc_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x18]);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(false, state.ps.carry);
}

#[test]
fn should_cycle_on_cld_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xd8]);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(false, state.ps.decimal);
}

#[test]
fn should_cycle_on_cli_implied_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x58]);
    cpu.ps.interrupt_disable = true;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(false, state.ps.interrupt_disable);
}

#[test]
fn should_cycle_on_clv_implied_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb8]);
    cpu.ps.overflow = true;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(false, state.ps.overflow);
}

#[test]
fn should_cycle_on_cmp_absolute_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xcd, 0xff, 0x00]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0xff),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cmp_absolute_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xcd, 0xff, 0x00]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0x00),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_absolute_indexed_with_x_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xdd, 0xfa, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0x00),
        )
        .with_gp_register(
            register::GPRegister::X,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_absolute_indexed_with_x_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xdd, 0xfa, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0xff),
        )
        .with_gp_register(
            register::GPRegister::X,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cmp_absolute_indexed_with_y_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd9, 0xfa, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0x00),
        )
        .with_gp_register(
            register::GPRegister::Y,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_absolute_indexed_with_y_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd9, 0xfa, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0xff),
        )
        .with_gp_register(
            register::GPRegister::Y,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cmp_indirect_y_indexed_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd1, 0x01, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0xea),
        )
        .with_gp_register(
            register::GPRegister::Y,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x01, 0xfa).unwrap();
    cpu.address_map.write(0x02, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cmp_indirect_y_indexed_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd1, 0x01, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0xff),
        )
        .with_gp_register(
            register::GPRegister::Y,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x01, 0xfa).unwrap();
    cpu.address_map.write(0x02, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_immediate_operation_with_inequal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc9, 0xff]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0x00),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_immediate_operation_with_equal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc9, 0xff]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0xff),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cmp_x_indexed_indirect_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xc1, 0x01, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0xea),
        )
        .with_gp_register(
            register::GPRegister::X,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x06, 0xff).unwrap();
    cpu.address_map.write(0x07, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cmp_x_indexed_indirect_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xc1, 0x01, 0x00])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0xff),
        )
        .with_gp_register(
            register::GPRegister::X,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x06, 0xff).unwrap();
    cpu.address_map.write(0x07, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_zeropage_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xc5, 0xff]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0x00),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_zeropage_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xc5, 0xff]).with_gp_register(
        register::GPRegister::ACC,
        register::GeneralPurpose::with_value(0xff),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cmp_zeropage_indexed_with_x_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd5, 0xfa])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0x00),
        )
        .with_gp_register(
            register::GPRegister::X,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cmp_zeropage_indexed_with_x_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xd5, 0xfa])
        .with_gp_register(
            register::GPRegister::ACC,
            register::GeneralPurpose::with_value(0xff),
        )
        .with_gp_register(
            register::GPRegister::X,
            register::GeneralPurpose::with_value(0x05),
        );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cpx_absolute_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xec, 0xff, 0x00]).with_gp_register(
        register::GPRegister::X,
        register::GeneralPurpose::with_value(0xff),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cpx_absolute_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xec, 0xff, 0x00]).with_gp_register(
        register::GPRegister::X,
        register::GeneralPurpose::with_value(0x00),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cpx_immediate_operation_with_inequal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xe0, 0xff]).with_gp_register(
        register::GPRegister::X,
        register::GeneralPurpose::with_value(0x00),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cpx_immediate_operation_with_equal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xe0, 0xff]).with_gp_register(
        register::GPRegister::X,
        register::GeneralPurpose::with_value(0xff),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cpx_zeropage_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xe4, 0xff]).with_gp_register(
        register::GPRegister::X,
        register::GeneralPurpose::with_value(0x00),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cpx_zeropage_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xe4, 0xff]).with_gp_register(
        register::GPRegister::X,
        register::GeneralPurpose::with_value(0xff),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cpy_absolute_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xcc, 0xff, 0x00]).with_gp_register(
        register::GPRegister::Y,
        register::GeneralPurpose::with_value(0xff),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cpy_absolute_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xcc, 0xff, 0x00]).with_gp_register(
        register::GPRegister::Y,
        register::GeneralPurpose::with_value(0x00),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cpy_immediate_operation_with_inequal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc0, 0xff]).with_gp_register(
        register::GPRegister::Y,
        register::GeneralPurpose::with_value(0x00),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cpy_immediate_operation_with_equal_operands() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc0, 0xff]).with_gp_register(
        register::GPRegister::Y,
        register::GeneralPurpose::with_value(0xff),
    );

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_cpy_zeropage_operation_with_inequal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xc4, 0xff]).with_gp_register(
        register::GPRegister::Y,
        register::GeneralPurpose::with_value(0x00),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, false, false)
    );
}

#[test]
fn should_cycle_on_cpy_zeropage_operation_with_equal_operands() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xc4, 0xff]).with_gp_register(
        register::GPRegister::Y,
        register::GeneralPurpose::with_value(0xff),
    );
    cpu.address_map.write(0x00ff, 0xff).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, true)
    );
}

#[test]
fn should_cycle_on_dec_absolute_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xce, 0xff, 0x01]);

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.address_map.read(0x01ff));
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_dec_absolute_indexed_with_x_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xde, 0xfa, 0x01])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));

    let state = cpu.run(7).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.address_map.read(0x01ff));
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_dec_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc6, 0xff]);

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.address_map.read(0xff));
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_dec_zeropage_indexed_with_x_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xd6, 0xfa])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.address_map.read(0xff));
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_dex_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xca])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x51));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0x50, state.x.read());
    assert_eq!((false, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_dey_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x88])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xfe, state.y.read());
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_eor_absolute_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x4d, 0xff, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_eor_absolute_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x5d, 0xfa, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_eor_absolute_indexed_with_y_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x59, 0xfa, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_eor_indirect_y_indexed_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x51, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_eor_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x49, 0x55])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_eor_x_indexed_indirect_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x41, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_eor_zeropage_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x45, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_eor_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x55, 0xfa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xaa, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_inc_absolute_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xee, 0xff, 0x01]);

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(1, state.address_map.read(0x01ff));
    assert_eq!((false, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_inc_absolute_indexed_with_x_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xfe, 0xfa, 0x01])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));

    let state = cpu.run(7).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(1, state.address_map.read(0x01ff));
    assert_eq!((false, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_inc_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xe6, 0xff]);

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(1, state.address_map.read(0xff));
    assert_eq!((false, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_inc_zeropage_indexed_with_x_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xf6, 0xfa])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(1, state.address_map.read(0xff));
    assert_eq!((false, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_inx_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xe8])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(127));
    assert_eq!(false, cpu.ps.negative);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(128, state.x.read());
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_iny_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xc8])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(127));
    assert_eq!(false, cpu.ps.negative);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(128, state.y.read());
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_jmp_absolute_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x4c, 0x50, 0x60]);

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6050, state.pc.read());
}

#[test]
fn should_cycle_on_jmp_indirect_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x6c, 0x50, 0x60]);
    let state = cpu.run(5).unwrap();

    assert_eq!(0xeaea, state.pc.read());
}

#[test]
fn should_cycle_on_jsr_absolute_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x20, 0x50, 0x60]);

    let sph = 0x1ffu16;
    let spl = 0x1feu16;
    let stack_base = 0x0100u16;

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6050, state.pc.read());
    assert_eq!(0x01fd, stack_base.wrapping_add(state.sp.read() as u16));
    assert_eq!(
        (0x02, 0x60),
        (state.address_map.read(spl), state.address_map.read(sph))
    )
}

#[test]
fn should_cycle_on_lda_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa9, 0xff, 0xa9, 0x0f]);

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6004, state.pc.read());
    assert_eq!(0x0f, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, false));
}

#[test]
fn should_cycle_on_lda_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa5, 0xff]);

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x00, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_lda_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb5, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_lda_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0xad, 0x00, 0x02])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();

    // val in mem should be null
    assert_eq!(0x00, state.acc.read());
    assert_eq!(0x00, state.address_map.read(0x0200));
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_lda_absolute_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xbd, 0x00, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_lda_absolute_indexed_with_y_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb9, 0x00, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_lda_indirect_y_indexed_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb1, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xea, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_lda_x_indexed_indirect_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xa1, 0x05])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x00));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xea).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xea, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ldx_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0xae, 0x00, 0x02])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();

    // val in mem should be null
    assert_eq!(0x00, state.x.read());
    assert_eq!(0x00, state.address_map.read(0x0200));
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_ldx_absolute_indexed_with_y_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xbe, 0x00, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ldx_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa2, 0xff]);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ldx_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa6, 0xff]);

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x00, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_ldx_zeropage_indexed_with_y_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb6, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ldy_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0xac, 0x00, 0x02])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();

    // val in mem should be null
    assert_eq!(0x00, state.x.read());
    assert_eq!(0x00, state.address_map.read(0x0200));
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_ldy_absolute_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xbc, 0x00, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ldy_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa0, 0xff]);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ldy_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa4, 0xff]);

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x00, state.y.read());
    assert_eq!((state.ps.negative, state.ps.zero), (false, true));
}

#[test]
fn should_cycle_on_ldy_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xb4, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_lsr_absolute_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x4e, 0xff, 0x00]);
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x2a, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, false)
    );
}

#[test]
fn should_cycle_on_lsr_absolute_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x5e, 0xfa, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(7).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x2a, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, false)
    );
}

#[test]
fn should_cycle_on_lsr_accumulator_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x4a])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x55));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0x2a, state.acc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, false)
    );
}

#[test]
fn should_cycle_on_lsr_zeropage_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x46, 0xff]);
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x2a, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, false)
    );
}

#[test]
fn should_cycle_on_lsr_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x56, 0xfa])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x2a, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (true, false, false)
    );
}

#[test]
fn should_cycle_on_nop_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![]);
    let state = cpu.run(3).unwrap();
    assert_eq!(0x6001, state.pc.read());

    // take 2 more cycles to validate ea has incremented again.
    let next_state = state.run(2).unwrap();
    assert_eq!(0x6002, next_state.pc.read());
}

#[test]
fn should_cycle_on_ora_absolute_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x0d, 0xff, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ora_absolute_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x1d, 0xfa, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ora_absolute_indexed_with_y_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x19, 0xfa, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ora_indirect_y_indexed_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x11, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ora_immediate_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x09, 0x55])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ora_x_indexed_indirect_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x01, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x55).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ora_zeropage_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x05, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_ora_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x15, 0xfa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_pha_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x48])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.address_map.read(0x01ff));
}

#[test]
fn should_cycle_on_php_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x08])
        .with_ps_register(register::ProcessorStatus::with_value(0x55));

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0x55, state.address_map.read(0x01ff));
}

#[test]
fn should_cycle_on_pla_implied_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x68])
        // simulate having pushed the value 0xff to the stack
        .with_sp_register(register::StackPointer::with_value(0xfe));
    cpu.address_map.write(0x01ff, 0xff).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!((true, false), (state.ps.negative, state.ps.zero));
}

#[test]
fn should_cycle_on_plp_implied_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x28])
        // simulate having pushed the value 0x55 from ps register to stack
        .with_sp_register(register::StackPointer::with_value(0xfe));
    cpu.address_map.write(0x01ff, 0x55).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0x55, state.ps.read());
}

#[test]
fn should_cycle_on_ror_absolute_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x6e, 0xff, 0x00]).with_ps_register({
        let mut ps = register::ProcessorStatus::default();
        ps.carry = true;
        ps
    });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xd5, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, true, false)
    );
}

#[test]
fn should_cycle_on_ror_absolute_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x7e, 0xfa, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = register::ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();

    let state = cpu.run(7).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xd5, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, true, false)
    );
}

#[test]
fn should_cycle_on_ror_accumulator_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x6a])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xaa))
        .with_ps_register({
            let mut ps = register::ProcessorStatus::default();
            ps.carry = true;
            ps
        });

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xd5, state.acc.read());
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, true, false)
    );
}

#[test]
fn should_cycle_on_ror_zeropage_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x66, 0xff]).with_ps_register({
        let mut ps = register::ProcessorStatus::default();
        ps.carry = true;
        ps
    });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xd5, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, true, false)
    );
}

#[test]
fn should_cycle_on_ror_zeropage_indexed_with_x_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x76, 0xfa])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = register::ProcessorStatus::default();
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xaa).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xd5, state.address_map.read(0x00ff));
    assert_eq!(
        (state.ps.carry, state.ps.negative, state.ps.zero),
        (false, true, false)
    );
}

#[test]
fn should_cycle_on_rts_implied_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x60])
        .with_sp_register(register::StackPointer::with_value(0xfd));
    cpu.address_map.write(0x01ff, 0x60).unwrap();
    cpu.address_map.write(0x01fe, 0x02).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0xff, state.sp.read());
    assert_eq!(0x6003, state.pc.read());
}

#[test]
fn should_cycle_on_sbc_absolute_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xed, 0xff, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_absolute_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xed, 0xff, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x80).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sbc_absolute_indexed_with_x_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xfd, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_absolute_indexed_with_x_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xfd, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x80).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sbc_absolute_indexed_with_y_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf9, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_absolute_indexed_with_y_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf9, 0xfa, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x80).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sbc_indirect_y_indexed_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf1, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xb0).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_indirect_y_indexed_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf1, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x80).unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sbc_immediate_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xe9, 0xb0])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });

    // set carry bit
    let mut ps = cpu.ps;
    ps.carry = true;
    cpu.ps = ps;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_immediate_operation_without_overflow() {
    let cpu = generate_test_cpu_with_instructions(vec![0xe9, 0x80])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sbc_x_indexed_indirect_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xe1, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0xb0).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_x_indexed_indirect_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xe1, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();
    cpu.address_map.write(0xff, 0x80).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sbc_zeropage_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xe5, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_zeropage_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xe5, 0xff])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00ff, 0x80).unwrap();

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sbc_zeropage_indexed_with_x_operation_with_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf5, 0xfa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x50))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_ps_register({
            let mut ps = MOS6502::default().ps;
            ps.carry = true;
            ps
        });
    cpu.address_map.write(0x00ff, 0xb0).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xa0, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (false, true, true, false)
    );
}

#[test]
fn should_cycle_on_sbc_zeropage_indexed_with_x_operation_without_overflow() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0xf5, 0xfa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));
    cpu.address_map.write(0x00ff, 0x80).unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0x7e, state.acc.read());
    assert_eq!(
        (
            state.ps.carry,
            state.ps.negative,
            state.ps.overflow,
            state.ps.zero
        ),
        (true, false, false, false)
    );
}

#[test]
fn should_cycle_on_sec_implied_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x38]);
    cpu.ps.carry = false;

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(true, state.ps.carry);
}

#[test]
fn should_cycle_on_sed_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xf8]);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(true, state.ps.decimal);
}

#[test]
fn should_cycle_on_sei_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x78]);

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(true, state.ps.interrupt_disable);
}

#[test]
fn should_cycle_on_sta_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0x8d, 0x00, 0x02])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0x0200));
}

#[test]
fn should_cycle_on_sta_absolute_indexed_with_x_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0x9d, 0x00, 0x02])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x5))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0x0205));
}

#[test]
fn should_cycle_on_sta_absolute_indexed_with_y_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0x99, 0x00, 0x02])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x5))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(5).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0x0205));
}

#[test]
fn should_cycle_on_sta_y_indexed_indirect_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x91, 0x00])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x00, 0xfa).unwrap();
    cpu.address_map.write(0x01, 0x00).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0xff));
}

#[test]
fn should_cycle_on_sta_x_indexed_indirect_operation() {
    let mut cpu = generate_test_cpu_with_instructions(vec![0x81, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));
    cpu.address_map.write(0x05, 0xff).unwrap();
    cpu.address_map.write(0x06, 0x00).unwrap();

    let state = cpu.run(6).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0xff));
}

#[test]
fn should_cycle_on_sta_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x85, 0x02])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0x02));
}

#[test]
fn should_cycle_on_sta_zeropage_with_x_index_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x95, 0x00])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05));

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.address_map.read(0x05));
}

#[test]
fn should_cycle_on_stx_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0x8e, 0x00, 0x02])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!(0xff, state.address_map.read(0x0200));
}

#[test]
fn should_cycle_on_stx_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x86, 0x02])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!(0xff, state.address_map.read(0x02));
}

#[test]
fn should_cycle_on_stx_zeropage_with_y_index_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x96, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x05));

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!(0xff, state.address_map.read(0x05));
}

#[test]
fn should_cycle_on_sty_absolute_operation() {
    let (ram_start, ram_end) = (0x0200, 0x5fff);
    let cpu = generate_test_cpu_with_instructions(vec![0x8c, 0x00, 0x02])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0xff))
        .register_address_space(
            ram_start..=ram_end,
            Memory::<ReadWrite>::new(ram_start, ram_end),
        )
        .unwrap();

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6003, state.pc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!(0xff, state.address_map.read(0x0200));
}

#[test]
fn should_cycle_on_sty_zeropage_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x84, 0x02])
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(3).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!(0xff, state.address_map.read(0x02));
}

#[test]
fn should_cycle_on_sty_zeropage_with_x_index_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x94, 0x00])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x05))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(4).unwrap();
    assert_eq!(0x6002, state.pc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!(0xff, state.address_map.read(0x05));
}

#[test]
fn should_cycle_on_tax_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xaa])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x00));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_tay_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xa8])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0xff))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0x00));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_tsx_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0xba])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x00))
        .with_sp_register(register::StackPointer::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.sp.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_txa_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x8a])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x00))
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.x.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}

#[test]
fn should_cycle_on_txs_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x9a])
        .with_gp_register(GPRegister::X, register::GeneralPurpose::with_value(0x00))
        .with_sp_register(register::StackPointer::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0x00, state.sp.read());
    assert_eq!(0x00, state.x.read());
}

#[test]
fn should_cycle_on_tya_implied_operation() {
    let cpu = generate_test_cpu_with_instructions(vec![0x98])
        .with_gp_register(GPRegister::ACC, register::GeneralPurpose::with_value(0x00))
        .with_gp_register(GPRegister::Y, register::GeneralPurpose::with_value(0xff));

    let state = cpu.run(2).unwrap();
    assert_eq!(0x6001, state.pc.read());
    assert_eq!(0xff, state.acc.read());
    assert_eq!(0xff, state.y.read());
    assert_eq!((state.ps.negative, state.ps.zero), (true, false));
}
