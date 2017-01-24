#[derive(Debug)]
pub struct PPU {
    pub ctrl: Ctrl,
    mask: Mask,
    pub status: u8,
    oamaddr: u8,
    oamdata: u8,
    scroll: u8,
    addr: u8,
    data: u8,
    oamdma: u8
}

impl PPU {
    pub fn new() -> PPU {
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
pub struct Ctrl {
    base_nametable_address: BaseNameTableAddress,
    vram_address_increment: VramAddressIncrement,
    sprite_pattern_table_address: PatternTableAddress,
    background_pattern_table_address: PatternTableAddress,
    sprite_size: SpriteSize,
    master_slave_select: MasterSlaveSelect,
    generate_nmi: GenerateMni
}

impl Ctrl {
    pub fn new() -> Ctrl {
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

    pub fn from_byte(byte: u8) -> Ctrl {
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

