use std::fmt;

use instruction::{Instruction,AddressMode};
use status_reg::StatusReg;
use interconnect::Interconnect;

pub struct Cpu {
    interconnect: Interconnect,
    pc: u16,
    accum: u8,
    x: u8,
    y: u8,
    status_reg: StatusReg,
    stack_pointer: u8
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
           "A: {:x}, X: {:x}, Y: {:x}, P: {:x}, SP: {:x}",
           self.accum, self.x, self.y, self.status_reg.as_byte(), self.stack_pointer
        )
    }
}

impl Cpu {
    pub fn new(rom: Vec<u8>) -> Cpu {
        Cpu {
            interconnect: Interconnect::new(rom),
            pc: 0x8000,
            accum: 0,
            x: 0,
            y: 0,
            status_reg: StatusReg::new(),
            stack_pointer: 0xFD
        }
    }

    pub fn run(&mut self) {
        loop {
            let raw_instruction = self.interconnect.read_byte(self.pc);
            let instruction = self.next_instruction();
            println!("{:x} {:x} {:?}", self.pc, raw_instruction, self);
            self.pc = self.execute_instruction(instruction);
        }
    }

    fn next_instruction(&mut self) -> Instruction {
        let raw_instruction = self.interconnect.read_byte(self.pc);
        match raw_instruction {
            0x9a => {
                Instruction::TransferXtoStackPointer
            }
            0xba => {
                Instruction::TransferStackPointerToX
            }
            0x4c => {
                let addr = self.absolute_address();
                Instruction::Jump(addr)
            }
            0x60 => {
                Instruction::ReturnFromSubRoutine
            }


            0xa9 => {
                let value = self.immediate_value();
                Instruction::LoadAccum(value)
            }
            0xad => {
                let addr = self.absolute_address();
                Instruction::LoadAccum(addr)
            }
            0xa5 => {
                let addr = self.zero_page_address();
                Instruction::LoadAccum(addr)
            }
            0xbd => {
                let addr = self.absolute_plus_x_address();
                Instruction::LoadAccum(addr)
            }
            0xa1 => {
                let addr = self.indirect_x_address();
                Instruction::LoadAccum(addr)
            }
            0x81 => {
                let addr = self.indirect_x_address();
                Instruction::StoreAccum(addr)
            }
            0x8d => {
                let addr = self.absolute_address();
                Instruction::StoreAccum(addr)
            }
            0x85 => {
                let addr = self.zero_page_address();
                Instruction::StoreAccum(addr)
            }


            0xa0 => {
                let value = self.immediate_value();
                Instruction::LoadY(value)
            }
            0xa4 => {
                let addr = self.zero_page_address();
                Instruction::LoadY(addr)
            }
            0xa2 => {
                let value = self.immediate_value();
                Instruction::LoadX(value)
            }
            0xa6 => {
                let addr = self.zero_page_address();
                Instruction::LoadX(addr)
            }
            0xae => {
                let addr = self.absolute_address();
                Instruction::LoadX(addr)
            }

            0x8e => {
                let addr = self.absolute_address();
                Instruction::StoreX(addr)
            }
            0x86 => {
                let addr = self.zero_page_address();
                Instruction::StoreX(addr)
            }

            0x84 => {
                let addr = self.zero_page_address();
                Instruction::StoreY(addr)
            }

            0x10 => {
                let addr = self.relative_address();
                Instruction::BranchOnPlus(addr)
            }
            0xf0 => {
                let addr = self.relative_address();
                Instruction::BranchOnEqual(addr)
            }
            0xb0 => {
                let addr = self.relative_address();
                Instruction::BranchOnCarry(addr)

            }
            0x20 => {
                let addr = self.absolute_address();
                Instruction::JumpToSubRoutine(addr)
            }
            0xea => {
                Instruction::Noop
            }
            0x18 => {
                Instruction::ClearCarry
            }
            0x78 => {
                Instruction::SetInterruptDisable
            }
            0xb8 => {
                Instruction::ClearOverflow
            }
            0xd8 => {
                Instruction::ClearDecimal
            }
            0x38 => {
                Instruction::SetCarry
            }
            0xf8 => {
                Instruction::SetDecimal
            }

            0x24 => {
                let addr = self.zero_page_address();
                Instruction::BitTest(addr)
            }

            0x30 => {
                let addr = self.relative_address();
                Instruction::BranchOnNegative(addr)
            }
            0x50 => {
                let addr = self.relative_address();
                Instruction::BranchOnOverflowClear(addr)
            }
            0x70 => {
                let addr = self.relative_address();
                Instruction::BranchOnOverflow(addr)
            }
            0x90 => {
                let addr = self.relative_address();
                Instruction::BranchOnCarryClear(addr)
            }
            0xd0 => {
                let addr = self.relative_address();
                Instruction::BranchOnNotEqual(addr)
            }

            0x8 => {
                Instruction::PushProcessorStatus
            }
            0x28 => {
                Instruction::PullProcessorStatus
            }
            0x48 => {
                Instruction::PushAccum
            }
            0x68 => {
                Instruction::PullAccum
            }

            0x61 => {
                let addr = self.indirect_x_address();
                Instruction::AddWithCarry(addr)
            }
            0x65 => {
                let addr = self.zero_page_address();
                Instruction::AddWithCarry(addr)
            }
            0x69 => {
                let value = self.immediate_value();
                Instruction::AddWithCarry(value)
            }
            0xe1 => {
                let addr = self.indirect_x_address();
                Instruction::SubtractWithCarry(addr)
            }
            0xe5 => {
                let addr = self.zero_page_address();
                Instruction::SubtractWithCarry(addr)
            }
            0xe9 => {
                let value = self.immediate_value();
                Instruction::SubtractWithCarry(value)
            }
            0x4a => {
                Instruction::LogicalShiftRight(AddressMode::Accumulator)
            }
            0x46 => {
                let addr = self.zero_page_address();
                Instruction::LogicalShiftRight(addr)
            }
            0x0a => {
                Instruction::ArithmeticShiftLeft(AddressMode::Accumulator)
            }
            0x6a => {
                Instruction::RotateRight(AddressMode::Accumulator)
            }
            0x2a => {
                Instruction::RotateLeft(AddressMode::Accumulator)
            }
            0xe8 => {
                Instruction::IncrementX
            }
            0xc8 => {
                Instruction::IncrementY
            }
            0xca => {
                Instruction::DecrementX
            }
            0x88 => {
                Instruction::DecrementY
            }
            0xaa => {
                Instruction::TransferAccumToX
            }
            0x8a => {
                Instruction::TransferXToAccum
            }
            0xa8 => {
                Instruction::TransferAccumToY
            }
            0x98 => {
                Instruction::TransferYToAccum
            }
            0x25 => {
                let addr = self.zero_page_address();
                Instruction::And(addr)
            }
            0x29 => {
                let value = self.immediate_value();
                Instruction::And(value)
            }
            0x21 => {
                let addr = self.indirect_x_address();
                Instruction::And(addr)
            }
            0x01 => {
                let addr = self.indirect_x_address();
                Instruction::Or(addr)
            }
            0x5 => {
                let addr = self.zero_page_address();
                Instruction::Or(addr)
            }
            0x09 => {
                let value = self.immediate_value();
                Instruction::Or(value)
            }
            0x41 => {
                let addr = self.indirect_x_address();
                Instruction::ExclusiveOr(addr)
            }
            0x45 => {
                let addr = self.zero_page_address();
                Instruction::ExclusiveOr(addr)
            }
            0x49 => {
                let value = self.immediate_value();
                Instruction::ExclusiveOr(value)
            }
            0xc0 => {
                let value = self.immediate_value();
                Instruction::CompareY(value)
            }
            0xc4 => {
                let addr = self.zero_page_address();
                Instruction::CompareY(addr)
            }
            0xe0 => {
                let value = self.immediate_value();
                Instruction::CompareX(value)
            }
            0xe4 => {
                let addr = self.zero_page_address();
                Instruction::CompareX(addr)
            }
            0xc1 => {
                let addr = self.indirect_x_address();
                Instruction::Compare(addr)
            }
            0xc5 => {
                let addr = self.zero_page_address();
                Instruction::Compare(addr)
            }
            0xc9 => {
                let value = self.immediate_value();
                Instruction::Compare(value)
            }

            0x40 => {
                Instruction::ReturnFromInterrupt
            }

            _ => {
                match 0xF & raw_instruction {
                    0x3 | 0x7 | 0xB | 0xF => panic!("Instructions cannot have low half byte equal to 3, 7, B, or F: {:x}", raw_instruction),
                    _ => panic!("Unrecognized instruction byte! {:x}", raw_instruction)
                }
            }
        }
    }

    fn immediate_value(&self) -> AddressMode {
        let value = self.interconnect.read_byte(self.pc + 1);
        AddressMode::Immediate(value)
    }

    fn absolute_address(&self) -> AddressMode {
        let addr_first = self.interconnect.read_byte(self.pc + 1) as u16;
        let addr_second = self.interconnect.read_byte(self.pc + 2) as u16;
        let addr = (addr_second << 8u16) + addr_first; // Second byte is the most signficant (i.e. little indian)
        AddressMode::Absolute(addr)
    }

    fn relative_address(&self) -> AddressMode {
        let offset = self.interconnect.read_byte(self.pc + 1);
        AddressMode::Relative(offset)
    }

    fn zero_page_address(&self) -> AddressMode {
        let addr = self.interconnect.read_byte(self.pc + 1);
        AddressMode::ZeroPage(addr)
    }

    fn absolute_plus_x_address(&self) -> AddressMode {
        let addr_first = self.interconnect.read_byte(self.pc + 1) as u16;
        let addr_second = self.interconnect.read_byte(self.pc + 2) as u16;
        let addr = (addr_second << 8u16) + addr_first; // Second byte is the most signficant (i.e. little indian)
        let x = self.x;
        AddressMode::AbsolutePlusX(addr, x)
    }

    fn indirect_x_address(&self) -> AddressMode {
        self.indirect_with_offset(self.x)
    }

    fn indirect_y_address(&self) -> AddressMode {
        self.indirect_with_offset(self.y)
    }

    fn indirect_with_offset(&self, offset: u8) -> AddressMode {
        let addr_addr_first = self.interconnect.read_byte(self.pc + 1 as u16) ;
        let addr_addr_second = addr_addr_first.wrapping_add(1);

        let addr_first = self.interconnect.read_byte(addr_addr_first.wrapping_add(offset) as u16) as u16;
        let addr_second = self.interconnect.read_byte(addr_addr_second.wrapping_add(offset) as u16) as u16;
        let addr = (addr_second << 8u16) + addr_first; // Second byte is the most signficant (i.e. little indian)

        AddressMode::IndirectX(addr)
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::Jump(AddressMode::Absolute(addr)) => {
                addr
            }
            Instruction::JumpToSubRoutine(AddressMode::Absolute(addr)) => {
                let next_instruction_addr = self.pc + 2;
                let next_instruction_addr_high_byte = ((next_instruction_addr & 0xFF00) >> 8) as u8;
                let next_instruction_addr_low_byte = (next_instruction_addr & 0xFF) as u8;
                self.push_on_stack(next_instruction_addr_high_byte);
                self.push_on_stack(next_instruction_addr_low_byte);
                addr
            }
            Instruction::ReturnFromSubRoutine => {
                let addr_low_byte = self.pop_off_stack() as u16;
                let addr_high_byte = self.pop_off_stack() as u16;
                let addr = (addr_high_byte << 8) | addr_low_byte;
                addr + 1
            }
            Instruction::SetInterruptDisable => {
                self.status_reg.interrupt = true;
                self.pc + 1
            }
            Instruction::ClearDecimal => {
                self.status_reg.decimal = false;
                self.pc + 1
            }
            Instruction::SetCarry => {
                self.status_reg.carry = true;
                self.pc + 1
            }
            Instruction::ClearCarry => {
                self.status_reg.carry = false;
                self.pc + 1
            }
            Instruction::SetDecimal => {
                self.status_reg.decimal = true;
                self.pc + 1
            }
            Instruction::ClearOverflow => {
                self.status_reg.overflow = false;
                self.pc + 1
            }
            Instruction::LoadAccum(addr) => {
                let(value, pc_offset) = match addr {
                    AddressMode::Absolute(addr) => {
                        let value = self.interconnect.read_byte(addr);
                        (value, 3)
                    }
                    AddressMode::IndirectX(addr) => {
                        let value = self.interconnect.read_byte(addr);
                        (value, 2)
                    }
                    AddressMode::Immediate(value) => {
                        (value, 2)
                    }
                    AddressMode::AbsolutePlusX(addr, offset) => {
                        let value = self.interconnect.read_byte(addr + offset as u16);
                        (value, 3)
                    }
                    AddressMode::ZeroPage(addr) => {
                        let value = self.interconnect.read_byte(addr as u16);
                        (value, 2)
                    }
                    _ => panic!("Unrecognized load accum addr {:?}", addr)
                };

                self.status_reg.zero = value == 0;
                self.status_reg.negative = (value >> 7) == 1;
                self.accum = value;
                self.pc + pc_offset
            }
            Instruction::StoreAccum(AddressMode::Absolute(addr)) => {
                self.interconnect.write_byte(addr, self.accum);
                self.pc + 3
            }
            Instruction::StoreAccum(AddressMode::ZeroPage(addr)) => {
                self.interconnect.write_byte(addr as u16, self.accum);
                self.pc + 2
            }
            Instruction::StoreAccum(AddressMode::IndirectX(addr)) => {
                self.interconnect.write_byte(addr, self.accum);
                self.pc + 2
            }
            Instruction::LoadX(AddressMode::Immediate(value)) => {
                self.status_reg.zero = value == 0;
                self.status_reg.negative = (value >> 7) == 1;
                self.x = value;
                self.pc + 2
            }
            Instruction::LoadX(AddressMode::Absolute(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.status_reg.zero = value == 0;
                self.status_reg.negative = (value >> 7) == 1;
                self.x = value;
                self.pc + 3
            }
            Instruction::LoadX(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.status_reg.zero = value == 0;
                self.status_reg.negative = (value >> 7) == 1;
                self.x = value;
                self.pc + 2
            }
            Instruction::LoadY(AddressMode::Immediate(value)) => {
                self.status_reg.zero = value == 0;
                self.status_reg.negative = (value >> 7) == 1;
                self.y = value;
                self.pc + 2
            }
            Instruction::LoadY(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.status_reg.zero = value == 0;
                self.status_reg.negative = (value >> 7) == 1;
                self.y = value;
                self.pc + 2
            }
            Instruction::StoreX(AddressMode::ZeroPage(addr)) => {
                let value = self.x;
                self.interconnect.write_byte(addr as u16, value);
                self.pc + 2
            }
            Instruction::StoreX(AddressMode::Absolute(addr)) => {
                let value = self.x;
                self.interconnect.write_byte(addr, value);
                self.pc + 3
            }
            Instruction::StoreY(AddressMode::ZeroPage(addr)) => {
                let value = self.y;
                self.interconnect.write_byte(addr as u16, value);
                self.pc + 2
            }
            Instruction::BranchOnNegative(AddressMode::Relative(offset)) => {
                self.branch(self.status_reg.negative, offset)
            }
            Instruction::BranchOnPlus(AddressMode::Relative(offset)) => {
                self.branch(!self.status_reg.negative, offset)
            }
            Instruction::BranchOnEqual(AddressMode::Relative(offset)) => {
                self.branch(self.status_reg.zero, offset)
            }
            Instruction::BranchOnNotEqual(AddressMode::Relative(offset)) => {
                self.branch(!self.status_reg.zero, offset)
            }
            Instruction::BranchOnCarry(AddressMode::Relative(offset)) => {
                self.branch(self.status_reg.carry, offset)
            }
            Instruction::BranchOnCarryClear(AddressMode::Relative(offset)) => {
                self.branch(!self.status_reg.carry, offset)
            }
            Instruction::BranchOnOverflow(AddressMode::Relative(offset)) => {
                self.branch(self.status_reg.overflow, offset)
            }
            Instruction::BranchOnOverflowClear(AddressMode::Relative(offset)) => {
                self.branch(!self.status_reg.overflow, offset)
            }
            Instruction::BitTest(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.status_reg.negative = (value >> 7) == 1;
                self.status_reg.overflow = ((value >> 6) & 0b1) == 1;
                let result = value & self.accum;
                self.status_reg.zero = result == 0;
                self.pc + 2
            }
            Instruction::AddWithCarry(AddressMode::IndirectX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.add_with_carry(value);
                self.pc + 2
            }
            Instruction::AddWithCarry(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.add_with_carry(value);
                self.pc + 2
            }
            Instruction::AddWithCarry(AddressMode::Immediate(value)) => {
                self.add_with_carry(value);
                self.pc + 2
            }
            Instruction::SubtractWithCarry(AddressMode::IndirectX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.subtract_with_carry(value);
                self.pc + 2
            }
            Instruction::SubtractWithCarry(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.subtract_with_carry(value);
                self.pc + 2
            }
            Instruction::SubtractWithCarry(AddressMode::Immediate(value)) => {
                self.subtract_with_carry(value);
                self.pc + 2
            }
            Instruction::LogicalShiftRight(AddressMode::Accumulator) => {
                let value = self.accum;
                self.logical_shift_right(value);
                self.pc + 1
            }
            Instruction::LogicalShiftRight(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.logical_shift_right(value);
                self.pc + 2
            }
            Instruction::ArithmeticShiftLeft(AddressMode::Accumulator) => {
                let value = self.accum;
                self.arithmetic_shift_left(value);
                self.pc + 1
            }
            Instruction::ArithmeticShiftLeft(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.arithmetic_shift_left(value);
                self.pc + 2
            }
            Instruction::RotateRight(AddressMode::Accumulator) => {
                let lowest_bit = self.accum & 0b1;
                let carry_bit = if self.status_reg.carry { 1 } else { 0 };
                let result = (self.accum >> 1) | (carry_bit << 7);
                self.accum = result;
                self.status_reg.carry = lowest_bit == 1;
                self.status_reg.zero = result == 0;
                self.status_reg.negative = result >> 7 == 1;
                self.pc + 1
            }
            Instruction::RotateLeft(AddressMode::Accumulator) => {
                let high_bit = self.accum >> 7;
                let carry_bit = if self.status_reg.carry { 1 } else { 0 };
                let result = (self.accum << 1) | carry_bit;
                self.accum = result;
                self.status_reg.carry = high_bit == 1;
                self.status_reg.zero = result == 0;
                self.status_reg.negative = result >> 7 == 1;
                self.pc + 1
            }
            Instruction::IncrementX => {
                self.x = self.x.wrapping_add(1);
                self.pc + 1
            }
            Instruction::IncrementY => {
                self.y = self.y.wrapping_add(1);
                self.pc + 1
            }
            Instruction::DecrementX => {
                self.x = self.x.wrapping_sub(1);
                self.pc + 1
            }
            Instruction::DecrementY => {
                self.y = self.y.wrapping_sub(1);
                self.pc + 1
            }
            Instruction::TransferStackPointerToX => {
                let result = self.stack_pointer;
                self.x = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            Instruction::TransferXtoStackPointer => {
                let result = self.x;
                self.stack_pointer = result;
                // No flags are set on this operation
                self.pc + 1
            }
            Instruction::TransferXToAccum => {
                let result = self.x;
                self.accum = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            Instruction::TransferAccumToX => {
                let result = self.accum;
                self.x = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            Instruction::TransferYToAccum => {
                let result = self.y;
                self.accum = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            Instruction::TransferAccumToY => {
                let result = self.accum;
                self.y = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            Instruction::And(AddressMode::Immediate(value)) => {
                self.and(value);
                self.pc + 2
            }
            Instruction::And(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.and(value);
                self.pc + 2
            }
            Instruction::And(AddressMode::IndirectX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.and(value);
                self.pc + 2
            }
            Instruction::Or(AddressMode::Immediate(value)) => {
                self.or(value);
                self.pc + 2
            }
            Instruction::Or(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.or(value);
                self.pc + 2
            }
            Instruction::Or(AddressMode::IndirectX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.or(value);
                self.pc + 2
            }
            Instruction::ExclusiveOr(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.exclusive_or(value);
                self.pc + 2
            }
            Instruction::ExclusiveOr(AddressMode::IndirectX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.exclusive_or(value);
                self.pc + 2
            }
            Instruction::ExclusiveOr(AddressMode::Immediate(value)) => {
                self.exclusive_or(value);
                self.pc + 2
            }
            Instruction::Compare(AddressMode::IndirectX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                let accum = self.accum;
                self.compare(accum, value);
                self.pc + 2
            }
            Instruction::Compare(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                let accum = self.accum;
                self.compare(accum, value);
                self.pc + 2
            }
            Instruction::Compare(AddressMode::Immediate(value)) => {
                let accum = self.accum;
                self.compare(accum, value);
                self.pc + 2
            }
            Instruction::CompareX(AddressMode::Immediate(value)) => {
                let x = self.x;
                self.compare(x, value);
                self.pc + 2
            }
            Instruction::CompareX(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                let x = self.x;
                self.compare(x, value);
                self.pc + 2
            }
            Instruction::CompareY(AddressMode::Immediate(value)) => {
                let y = self.y;
                self.compare(y, value);
                self.pc + 2
            }
            Instruction::CompareY(AddressMode::ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                let y = self.y;
                self.compare(y, value);
                self.pc + 2
            }
            Instruction::PushProcessorStatus => {
                let status = self.status_reg.as_byte();
                self.push_on_stack(status);
                self.pc + 1
            }
            Instruction::PullProcessorStatus => {
                let status = self.pop_off_stack();
                self.status_reg.set_byte(status);
                self.pc + 1
            }
            Instruction::PullAccum => {
                self.accum = self.pop_off_stack();
                self.status_reg.zero = self.accum == 0;
                self.status_reg.negative = (self.accum >> 7) == 1;
                self.pc + 1
            }
            Instruction::PushAccum => {
                let accum = self.accum;
                self.push_on_stack(accum);
                self.pc + 1
            }
            Instruction::ReturnFromInterrupt => {
                let status_reg = self.pop_off_stack();
                self.status_reg.set_byte(status_reg);
                let addr_low_byte = self.pop_off_stack() as u16;
                let addr_high_byte = self.pop_off_stack() as u16;
                let addr = (addr_high_byte << 8) | addr_low_byte;
                addr
            }
            Instruction::Noop => {
                self.pc + 1
            }
            _ => panic!("Unrecognized instruction {:?}", instruction)
        }
    }

    fn arithmetic_shift_left(&mut self, value: u8) {
        let highest_bit = value >> 7;
        let result = value << 1;
        self.accum = result;
        self.status_reg.carry = highest_bit == 1;
        self.status_reg.zero = result == 0;
        self.status_reg.negative = result >> 7 == 1;
    }

    fn logical_shift_right(&mut self, value: u8) {
        let lowest_bit = value & 0b1;
        let result = value >> 1;
        self.accum = result;
        self.status_reg.carry = lowest_bit == 1;
        self.status_reg.zero = result == 0;
        self.status_reg.negative = result >> 7 == 1;
    }

    fn add_with_carry(&mut self, value: u8) {
        let (mut result, did_overflow1) = self.accum.overflowing_add(value);
        let (mut result, did_overflow2) = result.overflowing_add(if self.status_reg.carry { 1 } else { 0 });
        if self.status_reg.decimal {
            println!("WRONG");
            //TODO: this is wrong
        }
        let accum_high_bit = self.accum >> 7;
        let value_high_bit = value >> 7;
        let result_high_bit = result >> 7;
        self.accum = result;
        self.status_reg.overflow = (accum_high_bit == value_high_bit) && (result_high_bit != accum_high_bit);
        self.status_reg.carry = did_overflow1 || did_overflow2;
        self.status_reg.zero = result == 0;
        self.status_reg.negative = result_high_bit == 1;
    }

    fn subtract_with_carry(&mut self, value: u8) {
        self.add_with_carry(!value)
    }

    fn compare(&mut self, stored_value: u8, value: u8) {
        let result = stored_value.wrapping_sub(value);
        self.status_reg.carry = stored_value >= value;
        self.status_reg.zero = stored_value == value;
        let highest_bit = result >> 7;
        self.status_reg.negative = highest_bit == 1;
    }

    fn and(&mut self, value: u8) {
        let result = value & self.accum;
        self.handle_result(result);
    }

    fn or(&mut self, value: u8) {
        let result = value | self.accum;
        self.handle_result(result);
    }

    fn exclusive_or(&mut self, value: u8) {
        let result = value ^ self.accum;
        self.handle_result(result);
    }

    fn handle_result(&mut self, result: u8) {
        self.accum = result;
        let highest_bit = result >> 7;
        self.status_reg.zero = highest_bit == 0;
        self.status_reg.negative = highest_bit == 1;
    }

    fn branch(&self, branch_condition: bool, offset: u8) -> u16 {
        if branch_condition {
            let negative = (offset >> 7) == 1;
            let value = (offset & 0b0111_1111) as u16;
            if negative {
                self.pc + 2 - value
            } else {
                self.pc + 2 + value
            }
        } else {
            self.pc + 2
        }
    }

    fn handle_transfer(&mut self, result: u8) {
        let highest_bit = result >> 7;
        self.status_reg.zero = result == 0;
        self.status_reg.negative = highest_bit == 1;
    }

    // TODO: Consider always inlining
    fn push_on_stack(&mut self, byte: u8) {
        self.interconnect.write_byte((self.stack_pointer - 1) as u16, byte);
        self.stack_pointer = self.stack_pointer - 1;
    }

    // TODO: Consider always inlining
    fn pop_off_stack(&mut self) -> u8 {
        let value = self.interconnect.read_byte(self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer + 1;
        value
    }
}
