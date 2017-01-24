use ppu::{PPU,Ctrl};

pub struct Interconnect {
    ram: [u8; 2 * 1024], // 2 Megabytes
    rom: Vec<u8>,
    ppu: PPU
}

impl Interconnect {
    pub fn new(mut rom: Vec<u8>) -> Interconnect {
        // TODO: parse rom header and make decisions based on it
        let pgr_rom = rom.split_off(0x10);
        Interconnect {
            ram: [0; 2 * 1024],
            rom: pgr_rom,
            ppu: PPU::new()
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
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
                let offset = a - 0x2000;
                match offset {
                    0x2 => self.ppu.status,
                    _ => panic!("reading from ppu offset {:x} is not supported", offset)
                }
            }
            _ => panic!("Reading byte at unrecognized addr 0x{:x}", addr)
        }
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            a if a < 0x7ff => self.ram[addr as usize] = byte,
            a if a >= 0x2000 && a <= 0x2007 => {
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

