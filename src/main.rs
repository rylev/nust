use std::env;
use std::fs::File;
use std::io::Read;


struct Cpu {
    rom: Vec<u8>,
    isp: u64
}

impl Cpu {
    fn new(rom: Vec<u8>) -> Cpu {
        Cpu {
            rom: rom,
            isp: 0
        }
    }

    fn run(&mut self) {
        loop {
            let instruction = self.rom[self.isp as usize];
            println!("Instruction: {:x}", instruction);
            self.isp += 1

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
