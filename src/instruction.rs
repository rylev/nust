#[derive(Debug)]
pub enum Instruction {
    SetInterruptDisable,
    ClearDecimal,
    SetCarry,
    ClearCarry,
    ClearOverflow,
    SetDecimal,

    TransferXtoStackPointer,

    Jump(AddressMode),
    JumpToSubRoutine(AddressMode),
    ReturnFromSubRoutine,

    LoadAccum(AddressMode),
    StoreAccum(AddressMode),
    LoadX(AddressMode),
    StoreX(AddressMode),
    LoadY(AddressMode),

    BranchOnPlus(AddressMode),
    BranchOnNegative(AddressMode),
    BranchOnEqual(AddressMode),
    BranchOnNotEqual(AddressMode),
    BranchOnCarry(AddressMode),
    BranchOnCarryClear(AddressMode),
    BranchOnOverflow(AddressMode),
    BranchOnOverflowClear(AddressMode),

    BitTest(AddressMode),

    IncrementY,
    AddWithCarry(AddressMode),
    SubtractWithCarry(AddressMode),
    And(AddressMode),
    Or(AddressMode),
    ExclusiveOr(AddressMode),
    Compare(AddressMode),
    CompareX(AddressMode),
    CompareY(AddressMode),

    PushProcessorStatus,
    PullProcessorStatus,
    PullAccum,
    PushAccum,

    Noop
}

#[derive(Debug)]
pub enum AddressMode {
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
