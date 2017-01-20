use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum Instruction {
    SetInterruptDisable,
    ClearDecimal,
    SetCarry,

    TransferXtoStackPointer,

    Jump(AddressMode),
    JumpToSubRoutine(AddressMode),
    LoadAccum(AddressMode),
    StoreAccum(AddressMode),
    LoadX(AddressMode),
    StoreX(AddressMode),
    BranchOnPlus(AddressMode),
    BranchOnEqual(AddressMode),
    BranchOnCarry(AddressMode),
    Noop
}

#[derive(Debug)]
enum AddressMode {
    Absolute(u16),
    AbsolutePlusX(u16, u8),
    AbsolutePlusY(u16, u8),
    ZeroPage(u8),
    ZeroPagePlusX(u8),
    ZeroPagePlusY(u8),
    Immediate(u8),
    Relative(u8),
    Indirect(u16),
    IndirectX(u8),
    IndirectY(u8)
}

/*
 * 7  bit  0
 * ---- ----
 * NVss DIZC
 * |||| ||||
 * |||| |||+- Carry: 1 if last addition or shift resulted in a carry, or if
 * |||| |||     last subtraction resulted in no borrow
 * |||| ||+-- Zero: 1 if last operation resulted in a 0 value
 * |||| |+--- Interrupt: Interrupt inhibit
 * |||| |       (0: /IRQ and /NMI get through; 1: only /NMI gets through)
 * |||| +---- Decimal: 1 to make ADC and SBC use binary-coded decimal arithmetic
 * ||||         (ignored on second-source 6502 like that in the NES)
 * ||++------ s: No effect, used by the stack copy, see note below
 * |+-------- Overflow: 1 if last ADC or SBC resulted in signed overflow,
 * |            or D6 from last BIT
 * +--------- Negative: Set to bit 7 of the last operation
 */
#[derive(Debug)]
struct StatusReg {
    carry: bool,
    zero: bool,
    interrupt: bool,
    decimal: bool,
    overflow: bool,
    negative: bool
}

impl StatusReg {
    fn new() -> StatusReg {
        StatusReg {
            carry: false,
            zero: false,
            interrupt: true,
            decimal: false,
            overflow: false,
            negative: false
        }
    }
}

#[derive(Debug)]
enum BaseNameTableAddress {
    Hex2000,
    Hex2400,
    Hex2800,
    Hex2C00
}
#[derive(Debug)]
enum VramAddressIncrement {
    Add1Across,
    Add32Down
}
#[derive(Debug)]
enum PatternTableAddress {
    Hex0,
    Hex1000
}
#[derive(Debug)]
enum SpriteSize {
    EightByEight,
    EightBySixteen
}
#[derive(Debug)]
enum MasterSlaveSelect {
    Read,
    Write
}
#[derive(Debug)]
enum GenerateMni {
    Off,
    On
}
#[derive(Debug)]
struct Ctrl {
    base_nametable_address: BaseNameTableAddress,
    vram_address_increment: VramAddressIncrement,
    sprite_pattern_table_address: PatternTableAddress,
    background_pattern_table_address: PatternTableAddress,
    sprite_size: SpriteSize,
    master_slave_select: MasterSlaveSelect,
    generate_nmi: GenerateMni
}

impl Ctrl {
    fn new() -> Ctrl {
        Ctrl {
            base_nametable_address: BaseNameTableAddress::Hex2000,
            vram_address_increment: VramAddressIncrement::Add1Across,
            sprite_pattern_table_address: PatternTableAddress::Hex0,
            background_pattern_table_address: PatternTableAddress::Hex0,
            sprite_size: SpriteSize::EightByEight,
            master_slave_select: MasterSlaveSelect::Read,
            generate_nmi: GenerateMni::Off
        }
    }

    fn from_byte(byte: u8) -> Ctrl {
        let base_nametable_address = match byte & 0b11000000 {
            0b00 => BaseNameTableAddress::Hex2000,
            0b01 => BaseNameTableAddress::Hex2400,
            0b10 => BaseNameTableAddress::Hex2800,
            _ => BaseNameTableAddress::Hex2C00,
        };
        let vram_address_increment = match byte & 0b100000 {
            0b0 => VramAddressIncrement::Add1Across,
            _ => VramAddressIncrement::Add32Down
        };
        let sprite_pattern_table_address = match byte & 0b10000 {
            0b0 => PatternTableAddress::Hex0,
            _ => PatternTableAddress::Hex1000
        };

        let background_pattern_table_address = match byte & 0b1000 {
            0b0 => PatternTableAddress::Hex0,
            _ => PatternTableAddress::Hex1000
        };

        let sprite_size = match byte & 0b100 {
            0b0 => SpriteSize::EightByEight,
            _ => SpriteSize::EightBySixteen,
        };

        let master_slave_select = match byte & 0b10 {
            0b0 => MasterSlaveSelect::Read,
            _ => MasterSlaveSelect::Write
        };

        let generate_nmi = match byte & 0b1 {
            0b0 => GenerateMni::Off,
            _ => GenerateMni::On
        };
        Ctrl {
            base_nametable_address: base_nametable_address,
            vram_address_increment: vram_address_increment,
            sprite_pattern_table_address: sprite_pattern_table_address,
            background_pattern_table_address: background_pattern_table_address,
            sprite_size: sprite_size,
            master_slave_select: master_slave_select,
            generate_nmi: generate_nmi

        }

    }
}

#[derive(Debug)]
struct Mask {
}

impl Mask {
    fn new() -> Mask {
        Mask {}
    }
}

#[derive(Debug)]
struct PPU {
    ctrl: Ctrl,
    mask: Mask,
    status: u8,
    oamaddr: u8,
    oamdata: u8,
    scroll: u8,
    addr: u8,
    data: u8,
    oamdma: u8
}

impl PPU {
    fn new() -> PPU {
        PPU {
            ctrl: Ctrl::new(),
            mask: Mask::new(),
            status: 0,
            oamaddr: 0,
            oamdata: 0,
            scroll: 0,
            addr: 0,
            data: 0,
            oamdma: 0
        }
    }
}

struct Interconnect {
    ram: [u8; 2 * 1024], // 2 Megabytes
    rom: Vec<u8>,
    ppu: PPU
}

impl Interconnect {
    fn new(mut rom: Vec<u8>) -> Interconnect {
        // TODO: parse rom header and make decisions based on it
        let pgr_rom = rom.split_off(0x10);
        Interconnect {
            ram: [0; 2 * 1024],
            rom: pgr_rom,
            ppu: PPU::new()
        }
    }

    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            a if a < 0x7ff => {
                self.ram[addr as usize]
            }
            a if a >= 0x8000 && a < 0xc000 => {
                self.rom[(addr - 0x8000) as usize]
            }
            a if a >= 0xc000 => {
                self.rom[(addr - 0xc000) as usize]
            }
            a if a >= 0x2000 && a <= 0x2007 => {
                println!("Reading from PPU at address: {:x}", a);
                let offset = a - 0x2000;
                match offset {
                    0x2 => self.ppu.status,
                    _ => panic!("reading from ppu offset {:x} is not supported", offset)
                }
            }
            _ => panic!("Reading byte at unrecognized addr 0x{:x}", addr)
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            a if a < 0x7ff => self.ram[addr as usize] = byte,
            a if a >= 0x2000 && a <= 0x2007 => {
                println!("Writing to PPU at address: {:x}", a);
                let offset = a - 0x2000;
                match offset {
                    0x0 => self.ppu.ctrl = Ctrl::from_byte(byte),
                    _ => panic!("writing to ppu offset {:x} is not supported", offset)
                };
            }
            _ => panic!("Writing addr at unrecognized addr 0x{:x}", addr)
        }
    }
}

struct Cpu {
    interconnect: Interconnect,
    pc: u16,
    accum: u8,
    x: u8,
    y: u8,
    status_reg: StatusReg,
    stack_pointer: u8
}
impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Cpu {{ pc: {:x}, accum: {}, x: {}, y: {}, stack_pointer: {}, status_reg: {:?} }}", self.pc, self.accum, self.x, self.y, self.stack_pointer, self.status_reg)
    }
}

impl Cpu {
    fn new(rom: Vec<u8>) -> Cpu {
        Cpu {
            interconnect: Interconnect::new(rom),
            pc: 0x8000,
            accum: 0,
            x: 0,
            y: 0,
            status_reg: StatusReg::new(),
            stack_pointer: 0
        }
    }

    fn run(&mut self) {
        loop {
            println!("Before: {:?}", self);
            let instruction = self.next_instruction();
            println!("Executing instruction: {:?}", instruction);
            self.pc = self.execute_instruction(instruction);
            println!("After: {:?}\n\n", self);
        }
    }

    fn next_instruction(&mut self) -> Instruction {
        let raw_instruction = self.interconnect.read_byte(self.pc);
        println!("Raw instruction: {:x}", raw_instruction);
        match raw_instruction {
            0x78 => {
                Instruction::SetInterruptDisable
            }
            0xd8 => {
                Instruction::ClearDecimal
            }
            0x9a => {
                Instruction::TransferXtoStackPointer
            }
            0x38 => {
                Instruction::SetCarry
            }
            0x4c => {
                let addr = self.absolute_address();
                Instruction::Jump(addr)
            }

            0xa9 => {
                let value = self.immediate_value();
                Instruction::LoadAccum(value)
            }
            0x8d => {
                let addr = self.absolute_address();
                Instruction::StoreAccum(addr)
            }

            0xad => {
                let addr = self.absolute_address();
                Instruction::LoadAccum(addr)
            }

            0xa2 => {
                let value = self.immediate_value();
                Instruction::LoadX(value)
            }

            0x86 => {
                let addr = self.zero_page();
                Instruction::StoreX(addr)
            }

            0x10 => {
                let addr = self.relative_address();
                Instruction::BranchOnPlus(addr)
            }
            0xbd => {
                let addr = self.absolute_plus_x_address();
                Instruction::LoadAccum(addr)
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

    fn zero_page(&self) -> AddressMode {
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

    fn execute_instruction(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::Jump(addr) => {
                match addr {
                    AddressMode::Absolute(addr) => addr,
                    _ => panic!("Unrecognized jump addr {:?}", addr)

                }
            }
            Instruction::JumpToSubRoutine(addr) => {
                match addr {
                    AddressMode::Absolute(addr) => {
                        // TODO: store next instruction in stack
                        addr
                    },
                    _ => panic!("Unrecognized jump addr {:?}", addr)

                }
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
            Instruction::LoadAccum(addr) => {
                let(value, pc_offset) = match addr {
                    AddressMode::Absolute(addr) => {
                        let value = self.interconnect.read_byte(addr);
                        (value, 3)
                    }
                    AddressMode::Immediate(value) => {
                        (value, 2)
                    }
                    AddressMode::AbsolutePlusX(addr, offset) => {
                        let value = self.interconnect.read_byte(addr + offset as u16);
                        (value, 3)
                    }
                    _ => panic!("Unrecognized load accum addr {:?}", addr)
                };

                self.status_reg.zero = value == 0;
                self.status_reg.negative = (value & 0x1) == 1;
                self.accum = value;
                self.pc + pc_offset
            }
            Instruction::StoreAccum(addr) => {
                match addr {
                    AddressMode::Absolute(addr) => {
                        self.interconnect.write_byte(addr, self.accum);
                        self.pc + 3
                    }
                    _ => panic!("Unrecognized store accum addr {:?}", addr)
                }
            }
            Instruction::LoadX(addr) => {
                match addr {
                    AddressMode::Immediate(value) => {
                        self.status_reg.zero = value == 0;
                        self.status_reg.negative = (value & 0x1) == 1;
                        self.x = value;
                        self.pc + 2
                    }
                    _ => panic!("Unrecognized load x addr {:?}", addr)
                }
            }
            Instruction::StoreX(addr) => {
                match addr {
                    AddressMode::ZeroPage(addr) => {
                        let value = self.x;
                        self.interconnect.write_byte(addr as u16, value);
                        self.pc + 2
                    }
                    _ => panic!("Unrecognized load x addr {:?}", addr)
                }
            }
            Instruction::TransferXtoStackPointer => {
                self.status_reg.zero = self.x == 0;
                self.status_reg.negative = (self.x & 0x1) == 1;
                self.stack_pointer = self.x;
                self.pc + 1
            }
            Instruction::BranchOnPlus(addr) => {
                match addr {
                    AddressMode::Relative(offset) => {
                        if self.status_reg.negative {
                            self.pc + 2
                        } else {
                            let negative = (0b1000_0000 & offset) == 1;
                            let value = (offset & 0b01111111) as u16;
                            if negative {
                                self.pc + 2 - value
                            } else {
                                self.pc + 2 + value
                            }
                        }
                    }
                    _ => panic!("Unrecognized branch of plus addr {:?}", addr)
                }
            }
            Instruction::BranchOnEqual(addr) => {
                match addr {
                    AddressMode::Relative(offset) => {
                        if self.status_reg.zero {
                            self.pc + 2 + (offset as u16)
                        } else {
                            self.pc + 2
                        }
                    }
                    _ => panic!("Unrecognized branch of equal addr {:?}", addr)
                }
            }
            Instruction::BranchOnCarry(addr) => {
                match addr {
                    AddressMode::Relative(offset) => {
                        if self.status_reg.carry {
                            self.pc + 2 + (offset as u16)
                        } else {
                            self.pc + 2
                        }
                    }
                    _ => panic!("Unrecognized branch of equal addr {:?}", addr)
                }
            }
            Instruction::Noop => {
                self.pc + 1
            }
        }
    }
}

fn main () {
    let rom_name = env::args().nth(1).expect("Expected the first argument to be the name of a rom");
    let mut rom_file = File::open(rom_name).expect("Problem opening rom. Does it exist?");
    let mut rom = vec![];
    let _ = rom_file.read_to_end(&mut rom).expect("Problem reading file");
    let mut cpu = Cpu::new(rom);
    cpu.run()
}
