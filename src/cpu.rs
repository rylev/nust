use std::fmt;

use instruction::{Instruction, AddressMode};
use instruction::Instruction::*;
use instruction::AddressMode::*;
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
                TransferXtoStackPointer
            }
            0xba => {
                TransferStackPointerToX
            }
            0x4c => {
                let addr = self.absolute_address();
                Jump(addr)
            }
            0x20 => {
                let addr = self.absolute_address();
                JumpToSubRoutine(addr)
            }
            0x6c => {
                let addr = self.indirect_address();
                Jump(addr)
            }
            0x60 => {
                ReturnFromSubRoutine
            }


            0xa9 => {
                let value = self.immediate_value();
                LoadAccum(value)
            }
            0xad => {
                let addr = self.absolute_address();
                LoadAccum(addr)
            }
            0xa5 => {
                let addr = self.zero_page_address();
                LoadAccum(addr)
            }
            0xb5 => {
                let addr = self.zero_page_plus_x_address();
                LoadAccum(addr)
            }
            0xbd => {
                let addr = self.absolute_plus_x_address();
                LoadAccum(addr)
            }
            0xb9 => {
                let addr = self.absolute_plus_y_address();
                LoadAccum(addr)
            }
            0xa1 => {
                let addr = self.indirect_x_address();
                LoadAccum(addr)
            }
            0xb1 => {
                let addr = self.indirect_y_address();
                LoadAccum(addr)
            }
            0x81 => {
                let addr = self.indirect_x_address();
                StoreAccum(addr)
            }
            0x8d => {
                let addr = self.absolute_address();
                StoreAccum(addr)
            }
            0x85 => {
                let addr = self.zero_page_address();
                StoreAccum(addr)
            }
            0x91 => {
                let addr = self.indirect_y_address();
                StoreAccum(addr)
            }
            0x95 => {
                let addr = self.zero_page_plus_x_address();
                StoreAccum(addr)
            }
            0x99 => {
                let addr = self.absolute_plus_y_address();
                StoreAccum(addr)
            }
            0x9d => {
                let addr = self.absolute_plus_x_address();
                StoreAccum(addr)
            }


            0xa0 => {
                let value = self.immediate_value();
                LoadY(value)
            }
            0xa4 => {
                let addr = self.zero_page_address();
                LoadY(addr)
            }
            0xac => {
                let addr = self.absolute_address();
                LoadY(addr)
            }
            0xb4 => {
                let addr = self.zero_page_plus_x_address();
                LoadY(addr)
            }
            0xbc => {
                let addr = self.absolute_plus_x_address();
                LoadY(addr)
            }
            0xa2 => {
                let value = self.immediate_value();
                LoadX(value)
            }
            0xa6 => {
                let addr = self.zero_page_address();
                LoadX(addr)
            }
            0xae => {
                let addr = self.absolute_address();
                LoadX(addr)
            }
            0xb6 => {
                let addr = self.zero_page_plus_y_address();
                LoadX(addr)
            }

            0x8e => {
                let addr = self.absolute_address();
                StoreX(addr)
            }
            0x86 => {
                let addr = self.zero_page_address();
                StoreX(addr)
            }
            0x96 => {
                let addr = self.zero_page_plus_y_address();
                StoreX(addr)
            }

            0x84 => {
                let addr = self.zero_page_address();
                StoreY(addr)
            }
            0x8c => {
                let addr = self.absolute_address();
                StoreY(addr)
            }
            0x94 => {
                let addr = self.zero_page_plus_x_address();
                StoreY(addr)
            }

            0x10 => {
                let addr = self.relative_address();
                BranchOnPlus(addr)
            }
            0xf0 => {
                let addr = self.relative_address();
                BranchOnEqual(addr)
            }
            0xb0 => {
                let addr = self.relative_address();
                BranchOnCarry(addr)

            }
            0xea => {
                Noop
            }
            0x18 => {
                ClearCarry
            }
            0x78 => {
                SetInterruptDisable
            }
            0xb8 => {
                ClearOverflow
            }
            0xd8 => {
                ClearDecimal
            }
            0x38 => {
                SetCarry
            }
            0xf8 => {
                SetDecimal
            }

            0x24 => {
                let addr = self.zero_page_address();
                BitTest(addr)
            }
            0x2c => {
                let addr = self.absolute_address();
                BitTest(addr)
            }

            0x30 => {
                let addr = self.relative_address();
                BranchOnNegative(addr)
            }
            0x50 => {
                let addr = self.relative_address();
                BranchOnOverflowClear(addr)
            }
            0x70 => {
                let addr = self.relative_address();
                BranchOnOverflow(addr)
            }
            0x90 => {
                let addr = self.relative_address();
                BranchOnCarryClear(addr)
            }
            0xd0 => {
                let addr = self.relative_address();
                BranchOnNotEqual(addr)
            }

            0x8 => {
                PushProcessorStatus
            }
            0x28 => {
                PullProcessorStatus
            }
            0x48 => {
                PushAccum
            }
            0x68 => {
                PullAccum
            }

            0x61 => {
                let addr = self.indirect_x_address();
                AddWithCarry(addr)
            }
            0x65 => {
                let addr = self.zero_page_address();
                AddWithCarry(addr)
            }
            0x69 => {
                let value = self.immediate_value();
                AddWithCarry(value)
            }
            0x6d => {
                let addr = self.absolute_address();
                AddWithCarry(addr)
            }
            0x71 => {
                let addr = self.indirect_y_address();
                AddWithCarry(addr)
            }
            0x75 => {
                let addr = self.zero_page_plus_x_address();
                AddWithCarry(addr)
            }
            0x79 => {
                let addr = self.absolute_plus_y_address();
                AddWithCarry(addr)
            }
            0x7d => {
                let addr = self.absolute_plus_x_address();
                AddWithCarry(addr)
            }
            0xe1 => {
                let addr = self.indirect_x_address();
                SubtractWithCarry(addr)
            }
            0xe5 => {
                let addr = self.zero_page_address();
                SubtractWithCarry(addr)
            }
            0xe9 => {
                let value = self.immediate_value();
                SubtractWithCarry(value)
            }
            0xed => {
                let addr = self.absolute_address();
                SubtractWithCarry(addr)
            }
            0xf1 => {
                let addr = self.indirect_y_address();
                SubtractWithCarry(addr)
            }
            0xf5 => {
                let addr = self.zero_page_plus_x_address();
                SubtractWithCarry(addr)
            }
            0xf9 => {
                let addr = self.absolute_plus_y_address();
                SubtractWithCarry(addr)
            }
            0xfd => {
                let addr = self.absolute_plus_x_address();
                SubtractWithCarry(addr)
            }
            0x4a => {
                LogicalShiftRight(Accumulator)
            }
            0x46 => {
                let addr = self.zero_page_address();
                LogicalShiftRight(addr)
            }
            0x4e => {
                let addr = self.absolute_address();
                LogicalShiftRight(addr)
            }
            0x56 => {
                let addr = self.zero_page_plus_x_address();
                LogicalShiftRight(addr)
            }
            0x5e => {
                let addr = self.absolute_plus_x_address();
                LogicalShiftRight(addr)
            }
            0x0a => {
                ArithmeticShiftLeft(Accumulator)
            }
            0x06 => {
                let addr = self.zero_page_address();
                ArithmeticShiftLeft(addr)
            }
            0x16 => {
                let addr = self.zero_page_plus_x_address();
                ArithmeticShiftLeft(addr)
            }
            0x0e => {
                let addr = self.absolute_address();
                ArithmeticShiftLeft(addr)
            }
            0x1e => {
                let addr = self.absolute_plus_x_address();
                ArithmeticShiftLeft(addr)
            }
            0x6a => {
                RotateRight(Accumulator)
            }
            0x66 => {
                let addr = self.zero_page_address();
                RotateRight(addr)
            }
            0x6e => {
                let addr = self.absolute_address();
                RotateRight(addr)
            }
            0x76 => {
                let addr = self.zero_page_plus_x_address();
                RotateRight(addr)
            }
            0x7e => {
                let addr = self.absolute_plus_x_address();
                RotateRight(addr)
            }
            0x2a => {
                RotateLeft(Accumulator)
            }
            0x26 => {
                let addr = self.zero_page_address();
                RotateLeft(addr)
            }
            0x2e => {
                let addr = self.absolute_address();
                RotateLeft(addr)
            }
            0x36 => {
                let addr = self.zero_page_plus_x_address();
                RotateLeft(addr)
            }
            0x3e => {
                let addr = self.absolute_plus_x_address();
                RotateLeft(addr)
            }
            0xe6 => {
                let addr = self.zero_page_address();
                Increment(addr)
            }
            0xee => {
                let addr = self.absolute_address();
                Increment(addr)
            }
            0xf6 => {
                let addr = self.zero_page_plus_x_address();
                Increment(addr)
            }
            0xfe => {
                let addr = self.absolute_plus_x_address();
                Increment(addr)
            }

            0xc6 => {
                let addr = self.zero_page_address();
                Decrement(addr)
            }
            0xce => {
                let addr = self.absolute_address();
                Decrement(addr)
            }
            0xd6 => {
                let addr = self.zero_page_plus_x_address();
                Decrement(addr)
            }
            0xde => {
                let addr = self.absolute_plus_x_address();
                Decrement(addr)
            }
            0xe8 => {
                IncrementX
            }
            0xc8 => {
                IncrementY
            }
            0xca => {
                DecrementX
            }
            0x88 => {
                DecrementY
            }
            0xaa => {
                TransferAccumToX
            }
            0x8a => {
                TransferXToAccum
            }
            0xa8 => {
                TransferAccumToY
            }
            0x98 => {
                TransferYToAccum
            }
            0x25 => {
                let addr = self.zero_page_address();
                And(addr)
            }
            0x29 => {
                let value = self.immediate_value();
                And(value)
            }
            0x21 => {
                let addr = self.indirect_x_address();
                And(addr)
            }
            0x2d => {
                let addr = self.absolute_address();
                And(addr)
            }
            0x3d => {
                let addr = self.absolute_plus_x_address();
                And(addr)
            }
            0x31 => {
                let addr = self.indirect_y_address();
                And(addr)
            }
            0x35 => {
                let addr = self.zero_page_plus_x_address();
                And(addr)
            }
            0x39 => {
                let addr = self.absolute_plus_y_address();
                And(addr)
            }
            0x01 => {
                let addr = self.indirect_x_address();
                Or(addr)
            }
            0x11 => {
                let addr = self.indirect_y_address();
                Or(addr)
            }
            0x5 => {
                let addr = self.zero_page_address();
                Or(addr)
            }
            0x09 => {
                let value = self.immediate_value();
                Or(value)
            }
            0x15 => {
                let addr = self.zero_page_plus_x_address();
                Or(addr)
            }
            0xd => {
                let addr = self.absolute_address();
                Or(addr)
            }
            0x19 => {
                let addr = self.absolute_plus_y_address();
                Or(addr)
            }
            0x1d => {
                let addr = self.absolute_plus_x_address();
                Or(addr)
            }
            0x41 => {
                let addr = self.indirect_x_address();
                ExclusiveOr(addr)
            }
            0x45 => {
                let addr = self.zero_page_address();
                ExclusiveOr(addr)
            }
            0x49 => {
                let value = self.immediate_value();
                ExclusiveOr(value)
            }
            0x4d => {
                let addr = self.absolute_address();
                ExclusiveOr(addr)
            }
            0x51 => {
                let addr = self.indirect_y_address();
                ExclusiveOr(addr)
            }
            0x55 => {
                let addr = self.zero_page_plus_x_address();
                ExclusiveOr(addr)
            }
            0x59 => {
                let addr = self.absolute_plus_y_address();
                ExclusiveOr(addr)
            }
            0x5d => {
                let addr = self.absolute_plus_x_address();
                ExclusiveOr(addr)
            }
            0xc0 => {
                let value = self.immediate_value();
                CompareY(value)
            }
            0xc4 => {
                let addr = self.zero_page_address();
                CompareY(addr)
            }
            0xcc => {
                let addr = self.absolute_address();
                CompareY(addr)
            }
            0xe0 => {
                let value = self.immediate_value();
                CompareX(value)
            }
            0xe4 => {
                let addr = self.zero_page_address();
                CompareX(addr)
            }
            0xec => {
                let addr = self.absolute_address();
                CompareX(addr)
            }
            0xc1 => {
                let addr = self.indirect_x_address();
                Compare(addr)
            }
            0xc5 => {
                let addr = self.zero_page_address();
                Compare(addr)
            }
            0xc9 => {
                let value = self.immediate_value();
                Compare(value)
            }
            0xcd => {
                let addr = self.absolute_address();
                Compare(addr)
            }
            0xd1 => {
                let addr = self.indirect_y_address();
                Compare(addr)
            }
            0xd5 => {
                let addr = self.zero_page_plus_x_address();
                Compare(addr)
            }
            0xd9 => {
                let addr = self.absolute_plus_y_address();
                Compare(addr)
            }
            0xdd => {
                let addr = self.absolute_plus_x_address();
                Compare(addr)
            }

            0x40 => {
                ReturnFromInterrupt
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
        Immediate(value)
    }

    fn absolute_address(&self) -> AddressMode {
        let addr = self.calculate_absolute_address();
        Absolute(addr)
    }

    fn absolute_plus_x_address(&self) -> AddressMode {
        let addr = self.calculate_absolute_address();
        let x = self.x as u16;
        AbsolutePlusX(addr.wrapping_add(x))
    }

    fn absolute_plus_y_address(&self) -> AddressMode {
        let addr = self.calculate_absolute_address();
        let y = self.y as u16;
        AbsolutePlusY(addr.wrapping_add(y))
    }

    fn calculate_absolute_address(&self) -> u16 {
        let addr_first = self.interconnect.read_byte(self.pc + 1) as u16;
        let addr_second = self.interconnect.read_byte(self.pc + 2) as u16;
        (addr_second << 8u16) | addr_first // Second byte is the most signficant (i.e. little indian)
    }

    fn relative_address(&self) -> AddressMode {
        let offset = self.interconnect.read_byte(self.pc + 1);
        Relative(offset)
    }

    fn zero_page_address(&self) -> AddressMode {
        let addr = self.interconnect.read_byte(self.pc + 1);
        ZeroPage(addr)
    }

    fn zero_page_plus_x_address(&self) -> AddressMode {
        let addr = self.interconnect.read_byte(self.pc.wrapping_add(1));
        ZeroPagePlusX(addr.wrapping_add(self.x))
    }

    fn zero_page_plus_y_address(&self) -> AddressMode {
        let addr = self.interconnect.read_byte(self.pc.wrapping_add(1));
        ZeroPagePlusY(addr.wrapping_add(self.y))
    }

    fn indirect_x_address(&self) -> AddressMode {
        let addr = self.calculate_indirect_address_with_offset(self.x);
        IndirectX(addr)
    }

    fn indirect_y_address(&self) -> AddressMode {
        let addr = self.calculate_indirect_address_with_offset(self.y);
        IndirectY(addr)
    }

    fn indirect_address(&self) -> AddressMode {
        let addr = self.calculate_indirect_address();
        Indirect(addr)
    }

    fn calculate_indirect_address(&self) -> u16 {
        let addr_addr_first = self.interconnect.read_byte(self.pc + 1 as u16) as u16;
        let addr_addr_second = self.interconnect.read_byte(self.pc + 2 as u16) as u16;

        let addr_addr = addr_addr_second << 8 | addr_addr_first;

        let addr_first = self.interconnect.read_byte(addr_addr) as u16;

        // This replicates a bug with the 6502. If the indirect address is on a
        // page boundry (i.e. it's least significant byte is 0xFF) then the address
        // used to fetch the most significant byte wraps around to the beginning
        // of the page
        let addr_second = if addr_addr_first == 0xFF {
            let faulty_address  = addr_addr_second << 8 | 0x00;
            self.interconnect.read_byte(faulty_address) as u16
        } else {
            self.interconnect.read_byte(addr_addr + 1) as u16
        };
        (addr_second << 8u16) | addr_first // Second byte is the most signficant (i.e. little indian)
    }

    fn calculate_indirect_address_with_offset(&self, offset: u8) -> u16 {
        let addr_addr_first = self.interconnect.read_byte(self.pc + 1 as u16);
        let addr_addr_second = addr_addr_first.wrapping_add(1);

        let addr_first = self.interconnect.read_byte(addr_addr_first as u16) as u16;
        let addr_second = self.interconnect.read_byte(addr_addr_second as u16) as u16;
        let addr = (addr_second << 8u16) | addr_first; // Second byte is the most signficant (i.e. little indian)

        addr.wrapping_add(offset as u16)
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Jump(Absolute(addr)) |
            Jump(Indirect(addr)) => {
                addr
            }
            JumpToSubRoutine(Absolute(addr)) => {
                let next_instruction_addr = self.pc + 2;
                let next_instruction_addr_high_byte = ((next_instruction_addr & 0xFF00) >> 8) as u8;
                let next_instruction_addr_low_byte = (next_instruction_addr & 0xFF) as u8;
                self.push_on_stack(next_instruction_addr_high_byte);
                self.push_on_stack(next_instruction_addr_low_byte);
                addr
            }
            ReturnFromSubRoutine => {
                let addr_low_byte = self.pop_off_stack() as u16;
                let addr_high_byte = self.pop_off_stack() as u16;
                let addr = (addr_high_byte << 8) | addr_low_byte;
                addr + 1
            }
            SetInterruptDisable => {
                self.status_reg.interrupt = true;
                self.pc + 1
            }
            ClearDecimal => {
                self.status_reg.decimal = false;
                self.pc + 1
            }
            SetCarry => {
                self.status_reg.carry = true;
                self.pc + 1
            }
            ClearCarry => {
                self.status_reg.carry = false;
                self.pc + 1
            }
            SetDecimal => {
                self.status_reg.decimal = true;
                self.pc + 1
            }
            ClearOverflow => {
                self.status_reg.overflow = false;
                self.pc + 1
            }
            LoadAccum(addr) => {
                let(value, pc_offset) = match addr {
                    Absolute(addr) |
                    AbsolutePlusX(addr) |
                    AbsolutePlusY(addr) => {
                        let value = self.interconnect.read_byte(addr);
                        (value, 3)
                    }
                    IndirectX(addr) |
                    IndirectY(addr) => {
                        let value = self.interconnect.read_byte(addr);
                        (value, 2)
                    }
                    Immediate(value) => {
                        (value, 2)
                    }
                    ZeroPage(addr) |
                    ZeroPagePlusX(addr) => {
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
            StoreAccum(Absolute(addr)) |
            StoreAccum(AbsolutePlusX(addr)) |
            StoreAccum(AbsolutePlusY(addr)) => {
                self.interconnect.write_byte(addr, self.accum);
                self.pc + 3
            }
            StoreAccum(ZeroPage(addr)) |
            StoreAccum(ZeroPagePlusX(addr)) => {
                self.interconnect.write_byte(addr as u16, self.accum);
                self.pc + 2
            }
            StoreAccum(IndirectX(addr)) |
            StoreAccum(IndirectY(addr)) => {
                self.interconnect.write_byte(addr, self.accum);
                self.pc + 2
            }
            LoadX(Immediate(value)) => {
                self.load_x(value);
                self.pc + 2
            }
            LoadX(ZeroPage(addr)) | LoadX(ZeroPagePlusY(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.load_x(value);
                self.pc + 2
            }
            LoadX(Absolute(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.load_x(value);
                self.pc + 3
            }
            LoadY(Immediate(value)) => {
                self.load_y(value);
                self.pc + 2
            }
            LoadY(ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.load_y(value);
                self.pc + 2
            }
            LoadY(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.load_y(value);
                self.pc + 2
            }
            LoadY(Absolute(addr)) |
            LoadY(AbsolutePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.load_y(value);
                self.pc + 3
            }
            StoreX(ZeroPage(addr)) |
            StoreX(ZeroPagePlusY(addr)) => {
                let value = self.x;
                self.interconnect.write_byte(addr as u16, value);
                self.pc + 2
            }
            StoreX(Absolute(addr)) => {
                let value = self.x;
                self.interconnect.write_byte(addr, value);
                self.pc + 3
            }
            StoreY(ZeroPage(addr)) => {
                let value = self.y;
                self.interconnect.write_byte(addr as u16, value);
                self.pc + 2
            }
            StoreY(ZeroPagePlusX(addr)) => {
                let value = self.y;
                self.interconnect.write_byte(addr as u16, value);
                self.pc + 2
            }
            StoreY(Absolute(addr)) => {
                let value = self.y;
                self.interconnect.write_byte(addr, value);
                self.pc + 3
            }
            BranchOnNegative(Relative(offset)) => {
                self.branch(self.status_reg.negative, offset)
            }
            BranchOnPlus(Relative(offset)) => {
                self.branch(!self.status_reg.negative, offset)
            }
            BranchOnEqual(Relative(offset)) => {
                self.branch(self.status_reg.zero, offset)
            }
            BranchOnNotEqual(Relative(offset)) => {
                self.branch(!self.status_reg.zero, offset)
            }
            BranchOnCarry(Relative(offset)) => {
                self.branch(self.status_reg.carry, offset)
            }
            BranchOnCarryClear(Relative(offset)) => {
                self.branch(!self.status_reg.carry, offset)
            }
            BranchOnOverflow(Relative(offset)) => {
                self.branch(self.status_reg.overflow, offset)
            }
            BranchOnOverflowClear(Relative(offset)) => {
                self.branch(!self.status_reg.overflow, offset)
            }
            BitTest(ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.bit_test(value);
                self.pc + 2
            }
            BitTest(Absolute(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.bit_test(value);
                self.pc + 3
            }
            AddWithCarry(IndirectX(addr)) |
            AddWithCarry(IndirectY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.add_with_carry(value);
                self.pc + 2
            }
            AddWithCarry(Absolute(addr)) |
            AddWithCarry(AbsolutePlusX(addr)) |
            AddWithCarry(AbsolutePlusY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.add_with_carry(value);
                self.pc + 3
            }
            AddWithCarry(ZeroPage(addr)) |
            AddWithCarry(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.add_with_carry(value);
                self.pc + 2
            }
            AddWithCarry(Immediate(value)) => {
                self.add_with_carry(value);
                self.pc + 2
            }
            SubtractWithCarry(IndirectX(addr)) |
            SubtractWithCarry(IndirectY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.subtract_with_carry(value);
                self.pc + 2
            }
            SubtractWithCarry(ZeroPage(addr)) |
            SubtractWithCarry(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.subtract_with_carry(value);
                self.pc + 2
            }
            SubtractWithCarry(Absolute(addr)) |
            SubtractWithCarry(AbsolutePlusX(addr)) |
            SubtractWithCarry(AbsolutePlusY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.subtract_with_carry(value);
                self.pc + 3
            }
            SubtractWithCarry(Immediate(value)) => {
                self.subtract_with_carry(value);
                self.pc + 2
            }
            LogicalShiftRight(Accumulator) => {
                let value = self.accum;
                self.logical_shift_right(value);
                self.pc + 1
            }
            LogicalShiftRight(ZeroPage(addr)) |
            LogicalShiftRight(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.logical_shift_right(value);
                self.pc + 2
            }
            LogicalShiftRight(Absolute(addr)) |
            LogicalShiftRight(AbsolutePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.logical_shift_right(value);
                self.pc + 3
            }
            ArithmeticShiftLeft(Accumulator) => {
                let value = self.accum;
                self.arithmetic_shift_left(value);
                self.pc + 1
            }
            ArithmeticShiftLeft(ZeroPage(addr)) |
            ArithmeticShiftLeft(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.arithmetic_shift_left(value);
                self.pc + 2
            }
            ArithmeticShiftLeft(Absolute(addr)) |
            ArithmeticShiftLeft(AbsolutePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.arithmetic_shift_left(value);
                self.pc + 3
            }
            RotateRight(Accumulator) => {
                let value = self.accum;
                self.rotate_right(value);
                self.pc + 1
            }
            RotateRight(ZeroPage(addr)) |
            RotateRight(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.rotate_right(value);
                self.pc + 2
            }
            RotateRight(Absolute(addr)) |
            RotateRight(AbsolutePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.rotate_right(value);
                self.pc + 3
            }
            RotateLeft(Accumulator) => {
                let value = self.accum;
                self.rotate_left(value);
                self.pc + 1
            }
            RotateLeft(ZeroPage(addr)) |
            RotateLeft(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.rotate_left(value);
                self.pc + 2
            }
            RotateLeft(Absolute(addr)) |
            RotateLeft(AbsolutePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.rotate_left(value);
                self.pc + 3
            }
            Increment(ZeroPage(addr)) |
            Increment(ZeroPagePlusX(addr)) => {
                self.increment(addr as u16);
                self.pc + 2
            }
            Increment(Absolute(addr)) |
            Increment(AbsolutePlusX(addr)) => {
                self.increment(addr);
                self.pc + 3
            }
            IncrementX => {
                self.x = self.x.wrapping_add(1);
                self.pc + 1
            }
            IncrementY => {
                self.y = self.y.wrapping_add(1);
                self.pc + 1
            }
            Decrement(ZeroPage(addr)) |
            Decrement(ZeroPagePlusX(addr)) => {
                self.decrement(addr as u16);
                self.pc + 2
            }
            Decrement(Absolute(addr)) |
            Decrement(AbsolutePlusX(addr)) => {
                self.decrement(addr);
                self.pc + 3
            }
            DecrementX => {
                self.x = self.x.wrapping_sub(1);
                self.pc + 1
            }
            DecrementY => {
                self.y = self.y.wrapping_sub(1);
                self.pc + 1
            }
            TransferStackPointerToX => {
                let result = self.stack_pointer;
                self.x = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            TransferXtoStackPointer => {
                let result = self.x;
                self.stack_pointer = result;
                // No flags are set on this operation
                self.pc + 1
            }
            TransferXToAccum => {
                let result = self.x;
                self.accum = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            TransferAccumToX => {
                let result = self.accum;
                self.x = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            TransferYToAccum => {
                let result = self.y;
                self.accum = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            TransferAccumToY => {
                let result = self.accum;
                self.y = result;
                self.handle_transfer(result);
                self.pc + 1
            }
            And(Immediate(value)) => {
                self.and(value);
                self.pc + 2
            }
            And(ZeroPage(addr)) | And(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.and(value);
                self.pc + 2
            }
            And(Absolute(addr)) |
            And(AbsolutePlusX(addr)) |
            And(AbsolutePlusY(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.and(value);
                self.pc + 3
            }
            And(IndirectX(addr)) | And(IndirectY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.and(value);
                self.pc + 2
            }
            Or(Immediate(value)) => {
                self.or(value);
                self.pc + 2
            }
            Or(ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.or(value);
                self.pc + 2
            }
            Or(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.or(value);
                self.pc + 2
            }
            Or(IndirectX(addr)) | Or(IndirectY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.or(value);
                self.pc + 2
            }
            Or(Absolute(addr)) |
            Or(AbsolutePlusX(addr)) |
            Or(AbsolutePlusY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.or(value);
                self.pc + 3
            }
            ExclusiveOr(ZeroPage(addr)) | ExclusiveOr(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.exclusive_or(value);
                self.pc + 2
            }
            ExclusiveOr(Absolute(addr)) |
            ExclusiveOr(AbsolutePlusX(addr)) |
            ExclusiveOr(AbsolutePlusY(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                self.exclusive_or(value);
                self.pc + 3
            }
            ExclusiveOr(IndirectX(addr)) | ExclusiveOr(IndirectY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                self.exclusive_or(value);
                self.pc + 2
            }
            ExclusiveOr(Immediate(value)) => {
                self.exclusive_or(value);
                self.pc + 2
            }
            Compare(IndirectX(addr)) | Compare(IndirectY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                let accum = self.accum;
                self.compare(accum, value);
                self.pc + 2
            }
            Compare(ZeroPage(addr)) | Compare(ZeroPagePlusX(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                let accum = self.accum;
                self.compare(accum, value);
                self.pc + 2
            }
            Compare(Absolute(addr)) |
            Compare(AbsolutePlusX(addr)) |
            Compare(AbsolutePlusY(addr)) => {
                let value = self.interconnect.read_byte(addr);
                let accum = self.accum;
                self.compare(accum, value);
                self.pc + 3
            }
            Compare(Immediate(value)) => {
                let accum = self.accum;
                self.compare(accum, value);
                self.pc + 2
            }
            CompareX(Immediate(value)) => {
                let x = self.x;
                self.compare(x, value);
                self.pc + 2
            }
            CompareX(ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                let x = self.x;
                self.compare(x, value);
                self.pc + 2
            }
            CompareX(Absolute(addr)) => {
                let value = self.interconnect.read_byte(addr);
                let x = self.x;
                self.compare(x, value);
                self.pc + 3
            }
            CompareY(Immediate(value)) => {
                let y = self.y;
                self.compare(y, value);
                self.pc + 2
            }
            CompareY(ZeroPage(addr)) => {
                let value = self.interconnect.read_byte(addr as u16);
                let y = self.y;
                self.compare(y, value);
                self.pc + 2
            }
            CompareY(Absolute(addr)) => {
                let value = self.interconnect.read_byte(addr);
                let y = self.y;
                self.compare(y, value);
                self.pc + 3
            }
            PushProcessorStatus => {
                let status = self.status_reg.as_byte();
                self.push_on_stack(status);
                self.pc + 1
            }
            PullProcessorStatus => {
                let status = self.pop_off_stack();
                self.status_reg.set_byte(status);
                self.pc + 1
            }
            PullAccum => {
                self.accum = self.pop_off_stack();
                self.status_reg.zero = self.accum == 0;
                self.status_reg.negative = (self.accum >> 7) == 1;
                self.pc + 1
            }
            PushAccum => {
                let accum = self.accum;
                self.push_on_stack(accum);
                self.pc + 1
            }
            ReturnFromInterrupt => {
                let status_reg = self.pop_off_stack();
                self.status_reg.set_byte(status_reg);
                let addr_low_byte = self.pop_off_stack() as u16;
                let addr_high_byte = self.pop_off_stack() as u16;
                let addr = (addr_high_byte << 8) | addr_low_byte;
                addr
            }
            Noop => {
                self.pc + 1
            }
            _ => panic!("Unrecognized instruction {:?}", instruction)
        }
    }

    fn load_x(&mut self, value: u8) {
        self.status_reg.zero = value == 0;
        self.status_reg.negative = (value >> 7) == 1;
        self.x = value;
    }

    fn load_y(&mut self, value: u8) {
        self.status_reg.zero = value == 0;
        self.status_reg.negative = (value >> 7) == 1;
        self.y = value;
    }

    fn increment(&mut self, addr: u16) {
        let value = self.interconnect.read_byte(addr);
        let result = value.wrapping_add(1);
        self.interconnect.write_byte(addr, result);
    }

    fn decrement(&mut self, addr: u16) {
        let value = self.interconnect.read_byte(addr);
        let result = value.wrapping_sub(1);
        self.interconnect.write_byte(addr, result);
    }

    fn bit_test(&mut self, value: u8) {
        self.status_reg.negative = (value >> 7) == 1;
        self.status_reg.overflow = ((value >> 6) & 0b1) == 1;
        let result = value & self.accum;
        self.status_reg.zero = result == 0;
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

    fn rotate_left(&mut self, value: u8) {
        let high_bit = value >> 7;
        let carry_bit = if self.status_reg.carry { 1 } else { 0 };
        let result = (value << 1) | carry_bit;
        self.accum = result;
        self.status_reg.carry = high_bit == 1;
        self.status_reg.zero = result == 0;
        self.status_reg.negative = result >> 7 == 1;
    }

    fn rotate_right(&mut self, value: u8) {
        let lowest_bit = value & 0b1;
        let carry_bit = if self.status_reg.carry { 1 } else { 0 };
        let result = (value >> 1) | (carry_bit << 7);
        self.accum = result;
        self.status_reg.carry = lowest_bit == 1;
        self.status_reg.zero = result == 0;
        self.status_reg.negative = result >> 7 == 1;
    }

    fn add_with_carry(&mut self, value: u8) {
        let (result, did_overflow1) = self.accum.overflowing_add(value);
        let (result, did_overflow2) = result.overflowing_add(if self.status_reg.carry { 1 } else { 0 });
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
