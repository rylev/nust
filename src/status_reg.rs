use std::fmt;

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
pub struct StatusReg {
    pub negative: bool,
    pub overflow: bool,
    pub decimal: bool,
    pub interrupt: bool,
    pub zero: bool,
    pub carry: bool
}

impl StatusReg {
    pub fn as_byte(&self) -> u8 {
        (if self.negative { 1 } else { 0 } << 7) |
        (if self.overflow { 1 } else { 0 } << 6) |
        (1 << 5) |
        (0 << 4) |
        (if self.decimal { 1 } else { 0 } << 3) |
        (if self.interrupt { 1 } else { 0 } << 2) |
        (if self.zero { 1 } else { 0 } << 1) |
        (if self.carry { 1 } else { 0 })
    }

    pub fn set_byte(&mut self, byte: u8) {
        self.negative = byte >> 7 == 1;
        self.overflow = ((byte >> 6) & 0b1) == 1;
        self.decimal = ((byte >> 3) & 0b1) == 1;
        self.interrupt = ((byte >> 2) & 0b1) == 1;
        self.zero = ((byte >> 1) & 0b1) == 1;
        self.carry = (byte & 0b1) == 1;
    }
}

impl fmt::Debug for StatusReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "StatusReg({:x})",
            self.as_byte()
            // "StatusReg {{ negative: {}, overflow: {}, decimal: {}, interrupt: {}, zero: {}, carry: {} }} ({:x})",
            // self.negative, self.overflow, self.decimal, self.interrupt, self.zero, self.carry, self.as_byte()
        )
    }
}

impl StatusReg {
    pub fn new() -> StatusReg {
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


