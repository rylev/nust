use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum Instruction {
    SetInterruptDisable,
    ClearDecimal,

    TransferXtoStackPointer,

    Jump(AddressMode),
    LoadAccum(AddressMode),
    StoreAccum(AddressMode),
    LoadX(AddressMode),
    BranchOnPlus(AddressMode)
}

#[derive(Debug)]
enum AddressMode {
    Absolute(u16),
    AbsolutePlusX(u16),
    AbsolutePlusY(u16),
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

struct Interconnect {
    ram: [u8; 2 * 1024], // 2 Megabytes
    rom: Vec<u8>
}

impl Interconnect {
    fn new(rom: Vec<u8>) -> Interconnect {
        Interconnect {
            ram: [0; 2 * 1024],
            rom: rom
        }
    }

    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            a if a < 0x7ff => self.ram[addr as usize],
            a if a > 0x8000 => self.rom[(addr - 0x8000) as usize],
            _ => panic!("Reading byte at unrecognized addr 0x{:x}", addr)
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            a if a < 0x7ff => self.ram[addr as usize] = byte,
            a if a >= 0x2000 && a <= 0x2007 => println!("TODO: implement writing to PPU"),
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

impl Cpu {
    fn new(rom: Vec<u8>) -> Cpu {
        Cpu {
            interconnect: Interconnect::new(rom),
            pc: (0x8000 + 0x10), // Roms starts at 0x8000 and the header is 0x10 bytes big
            accum: 0,
            x: 0,
            y: 0,
            status_reg: StatusReg::new(),
            stack_pointer: 0
        }
    }

    fn run(&mut self) {
        loop {
            let instruction = self.next_instruction();
            println!("Executing instruction: {:?}", instruction);
            self.pc = self.execute_instruction(instruction)
        }
    }

    fn next_instruction(&mut self) -> Instruction {
        let raw_instruction = self.interconnect.read_byte(self.pc);
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
            0x4c => {
                let addr_first = self.interconnect.read_byte(self.pc + 1) as u16;
                let addr_second = self.interconnect.read_byte(self.pc + 2) as u16;
                let addr = (addr_second << 8u16) + addr_first; // Second byte is the most signficant (i.e. little indian)
                println!("Combining {:x} and {:x} to jump to {:x}", addr_second, addr_first, addr);
                Instruction::Jump(AddressMode::Absolute(addr))
            }
            0xa9 => {
                let value = self.interconnect.read_byte(self.pc + 1);
                Instruction::LoadAccum(AddressMode::Immediate(value))
            }
            0x8d => {
                let addr_first = self.interconnect.read_byte(self.pc + 1) as u16;
                let addr_second = self.interconnect.read_byte(self.pc + 2) as u16;
                let addr = (addr_second << 8u16) + addr_first; // Second byte is the most signficant (i.e. little indian)
                Instruction::StoreAccum(AddressMode::Absolute(addr))
            }
            0xa2 => {
                let value = self.interconnect.read_byte(self.pc + 1);
                Instruction::LoadX(AddressMode::Immediate(value))
            }
            0xad => {
                let addr_first = self.interconnect.read_byte(self.pc + 1) as u16;
                let addr_second = self.interconnect.read_byte(self.pc + 2) as u16;
                let addr = (addr_second << 8u16) + addr_first; // Second byte is the most signficant (i.e. little indian)
                Instruction::LoadAccum(AddressMode::Absolute(addr))
            }
            0x10 => {
                let offset = self.interconnect.read_byte(self.pc + 1);
                Instruction::BranchOnPlus(AddressMode::Relative(offset))
            }
            _ => panic!("Unrecognized instruction byte! {:x}", raw_instruction)
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::Jump(addr) => {
                match addr {
                    AddressMode::Absolute(addr) => addr,
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
            Instruction::LoadAccum(addr) => {
                // TODO: This should effect the neagtive and zero flags
                match addr {
                    AddressMode::Absolute(addr) => {
                        let value = self.interconnect.read_byte(addr);
                        self.accum = value;
                        self.pc + 3
                    }
                    AddressMode::Immediate(value) => {
                        self.accum = value;
                        self.pc + 2
                    }
                    _ => panic!("Unrecognized jump addr {:?}", addr)

                }
            }
            Instruction::StoreAccum(addr) => {
                match addr {
                    AddressMode::Absolute(addr) => {
                        self.interconnect.write_byte(addr, self.accum);
                        self.pc + 3
                    }
                    _ => panic!("Unrecognized jump addr {:?}", addr)
                }
            }
            Instruction::LoadX(addr) => {
                // TODO: This should effect the neagtive and zero flags
                match addr {
                    AddressMode::Immediate(value) => {
                        self.x = value;
                        self.pc + 2
                    }
                    _ => panic!("Unrecognized jump addr {:?}", addr)
                }
            }
            Instruction::TransferXtoStackPointer => {
                // TODO: This should effect the neagtive and zero flags
                self.stack_pointer = self.x;
                self.pc + 1
            }
            Instruction::BranchOnPlus(addr) => {
                match addr {
                    AddressMode::Relative(offset) => {
                        self.pc + 1 + (offset as u16)
                    }
                    _ => panic!("Unrecognized jump addr {:?}", addr)
                }
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
