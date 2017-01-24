mod instruction;
mod status_reg;
mod ppu;
mod interconnect;
mod cpu;

use std::env;
use std::fs::File;
use std::io::Read;

use cpu::Cpu;

fn main () {
    let rom_name = env::args().nth(1).expect("Expected the first argument to be the name of a rom");
    let mut rom_file = File::open(rom_name).expect("Problem opening rom. Does it exist?");
    let mut rom = vec![];
    let _ = rom_file.read_to_end(&mut rom).expect("Problem reading file");
    let mut cpu = Cpu::new(rom);
    cpu.run()
}
