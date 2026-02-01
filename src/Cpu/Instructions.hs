module Cpu.Instructions where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu

-- | Address offsets for addressing modes.
-- Details of interpretation depend on addressing mode.
data AddressOffset
  = -- | No offset.
    None
  | -- | Offset is given by X register.
    XRegOffset
  | -- | Offset is given by Y register.
    YRegOffset
  deriving (Eq, Show, Generic, NFDataX)

-- | Defines possible instruction addressing modes.
data AddressingMode
  = -- | Operation on value provided in instruction.
    Immediate
  | -- | Operation on memory, absolute address given.
    Absolute AddressOffset
  | -- | Operation on memory addressed indirectly.
    -- Instruction provides absolute address of target-address location.
    Indirect AddressOffset
  | -- | Operation on zero-page memory.
    ZeroPage AddressOffset
  | -- | Stack Pointer addressing.
    -- Note that this is not one of the official modes available on MOS6502.
    StackPointer
  deriving (Eq, Show, Generic, NFDataX)

undefinedAddressingMode :: AddressingMode
undefinedAddressingMode = errorX "Undefined addressing mode"

-- | Branch conditions for BRANCH instruction.
data BranchCondition
  = -- | Take branch when N flag is 0.
    OnPlus
  | -- | Take branch when N flag is 1.
    OnMinus
  | -- | Take branch when V flag is 0.
    OnOverflowClear
  | -- | Take branch when V flag is 1.
    OnOverflowSet
  | -- | Take branch when C flag is 0.
    OnCarryClear
  | -- | Take branch when C flag is 1.
    OnCarrySet
  | -- | Take branch when Z flag is 0.
    OnNotEqual
  | -- | Take branch when Z flag is 1.
    OnEqual
  deriving (Eq, Show, Generic, NFDataX)

data ALUIO = RegA | RegX | RegY | RegSP | Memory | One
  deriving (Show, Eq, Generic, NFDataX)

data ALUConnect = ALUConnect
  { _left :: Maybe ALUIO,
    _right :: ALUIO,
    _output :: Maybe ALUIO
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Represents all possible MOS6502 instructions.
--
-- *Relaxed* type restrictions allow for building non-existing instructions.
-- This should not be a problem as such instructions can be marked as 'errorX' during microcode generation.
data Instruction
  = -- | No Operation.
    NOP
  | -- | Push Status register onto stack.
    PHP
  | -- | Pull Status register from stack.
    PLP
  | {- Flag operations -}

    -- | Set Carry Flag.
    SEC
  | -- | Clear Carry Flag.
    CLC
  | -- | Set Decimal Flag.
    SED
  | -- | Clear Decimal Flag.
    CLD
  | -- | Clear Overflow Flag.
    CLV
  | -- | Set Interrupt Disable Flag.
    SEI
  | -- | Clear Interrupt Disable Flag.
    CLI
  | {- Subroutines -}

    -- | Jump to Subroutine: Jump to new location saving return address on the Stack.
    JSR
  | -- | Return from Subroutine: Pull the saved address of return point from the Stack.
    RTS
  | -- | Force Break: Initiate software interrupt.
    --
    -- Pushes PC and Status registers onto the stack,
    -- then loads the next instruction address from an interrupt vector.
    BRK
  | -- | Return From Interrupt.
    --
    -- Pull Status register and PC from Stack.
    RTI
  | {- Jump Operations -}

    -- | Jump to new location.
    JMP
  | -- | Jump to relative address if condition is true.
    BRANCH BranchCondition
  | {- Arithmetic Operations -}

    -- | Perform arithmetic operation.
    --
    -- The 'ALU' operation will be performed on chosen values (registers or memory).
    -- Loads from memory will use 'AddressingMode'.
    -- Calculation will update flags and save the result to chosen destination.
    --
    -- Note that for operations that do not use memory the 'AddressingMode' should be 'undefinedAddressingMode'.
    Compute ALU ALUConnect Bool
  deriving (Eq, Show, Generic, NFDataX)

decodeBranch :: BitVector 3 -> BranchCondition
decodeBranch = \case
  0b000 -> OnPlus
  0b001 -> OnMinus
  0b010 -> OnOverflowClear
  0b011 -> OnOverflowSet
  0b100 -> OnCarryClear
  0b101 -> OnCarrySet
  0b110 -> OnNotEqual
  0b111 -> OnEqual
  _ -> errorX "Impossible: decodeBranch"

decodeAluOp :: BitVector 3 -> ALU
decodeAluOp = \case
  0b000 -> BinaryOp OR
  0b001 -> BinaryOp AND
  0b010 -> BinaryOp XOR
  0b011 -> ADC
  0b100 -> errorX "Should not decode STA as aluOp"
  0b101 -> ID
  0b110 -> CMP
  0b111 -> SBC
  _ -> errorX "Impossible: decodeAluOp"

decodeAluAddressing :: BitVector 3 -> AddressingMode
decodeAluAddressing = \case
  0b000 -> Indirect XRegOffset
  0b001 -> ZeroPage None
  0b010 -> Immediate
  0b011 -> Absolute None
  0b100 -> Indirect YRegOffset
  0b101 -> ZeroPage XRegOffset
  0b110 -> Absolute YRegOffset
  0b111 -> Absolute XRegOffset
  _ -> errorX "Impossible: decodeAluAddressing"

decodeStoreAddressing :: AddressOffset -> BitVector 2 -> AddressingMode
decodeStoreAddressing memoryTarget = \case
  0b00 -> ZeroPage None
  0b01 -> Absolute None
  0b10 -> ZeroPage memoryTarget
  0b11 -> errorX "Undefined addressing bits for ST instruction"
  _ -> errorX "Impossible: storeAddressing"

decodeShiftOp :: BitVector 2 -> ALUShiftOp
decodeShiftOp = \case
  0b00 -> ASL
  0b01 -> ROL
  0b10 -> LSR
  0b11 -> ROR
  _ -> errorX "Impossible: shiftOp"

decodeIncDecAddressing :: AddressOffset -> BitVector 2 -> AddressingMode
decodeIncDecAddressing memoryTarget = \case
  0b00 -> ZeroPage None
  0b01 -> Absolute None
  0b10 -> ZeroPage memoryTarget
  0b11 -> Absolute memoryTarget
  _ -> errorX "Impossible: incDecAddressing"

decodeLoadAddressing :: BitVector 2 -> AddressingMode
decodeLoadAddressing = \case
  0b00 -> Immediate
  0b01 -> ZeroPage None
  0b10 -> errorX "Undefined loadAddressing"
  0b11 -> Absolute None
  _ -> errorX "Impossible: loadAddressing"

-- | Decode first byte of the instruction into 'Instruction' type.
decode :: Data -> (Instruction, AddressingMode)
decode op = case op of
  $(bitPattern "11101010") -> (NOP, undefinedAddressingMode)
  $(bitPattern "00001000") -> (PHP, undefinedAddressingMode)
  $(bitPattern "00101000") -> (PLP, undefinedAddressingMode)
  $(bitPattern "00111000") -> (SEC, undefinedAddressingMode)
  $(bitPattern "00011000") -> (CLC, undefinedAddressingMode)
  $(bitPattern "11111000") -> (SED, undefinedAddressingMode)
  $(bitPattern "11011000") -> (CLD, undefinedAddressingMode)
  $(bitPattern "10111000") -> (CLV, undefinedAddressingMode)
  $(bitPattern "01111000") -> (SEI, undefinedAddressingMode)
  $(bitPattern "01011000") -> (CLI, undefinedAddressingMode)
  $(bitPattern "00100000") -> (JSR, undefinedAddressingMode)
  $(bitPattern "01100000") -> (RTS, undefinedAddressingMode)
  $(bitPattern "00000000") -> (BRK, undefinedAddressingMode)
  $(bitPattern "01000000") -> (RTI, undefinedAddressingMode)
  $(bitPattern "01.01100") -> (JMP, jumpAddressing)
  $(bitPattern "...10000") -> (BRANCH branch, undefinedAddressingMode)
  -- Store register register to memory.
  $(bitPattern "010...01") -> (Compute ID ALUConnect {_left = Nothing, _right = RegA, _output = Just Memory} False, aluAddressing)
  $(bitPattern "100.0100") -> (Compute ID ALUConnect {_left = Nothing, _right = RegY, _output = Just Memory} False, storeAddressing XRegOffset)
  $(bitPattern "100.0110") -> (Compute ID ALUConnect {_left = Nothing, _right = RegX, _output = Just Memory} False, storeAddressing YRegOffset)
  $(bitPattern "10001100") -> (Compute ID ALUConnect {_left = Nothing, _right = RegY, _output = Just Memory} False, storeAddressing XRegOffset)
  $(bitPattern "10001110") -> (Compute ID ALUConnect {_left = Nothing, _right = RegX, _output = Just Memory} False, storeAddressing YRegOffset)
  -- PHA, PLA.
  $(bitPattern "01001000") -> (Compute ID ALUConnect {_left = Nothing, _right = RegA, _output = Just Memory} False, StackPointer)
  $(bitPattern "01101000") -> (Compute ID ALUConnect {_left = Nothing, _right = Memory, _output = Just RegA} True, StackPointer)
  -- Transfer Register.
  $(bitPattern "10001010") -> (Compute ID ALUConnect {_left = Nothing, _right = RegX, _output = Just RegA} True, undefinedAddressingMode)
  $(bitPattern "10101010") -> (Compute ID ALUConnect {_left = Nothing, _right = RegA, _output = Just RegX} True, undefinedAddressingMode)
  $(bitPattern "10111010") -> (Compute ID ALUConnect {_left = Nothing, _right = RegSP, _output = Just RegX} True, undefinedAddressingMode)
  $(bitPattern "10011010") -> (Compute ID ALUConnect {_left = Nothing, _right = RegX, _output = Just RegSP} False, undefinedAddressingMode)
  $(bitPattern "10011000") -> (Compute ID ALUConnect {_left = Nothing, _right = RegY, _output = Just RegA} True, undefinedAddressingMode)
  $(bitPattern "10101000") -> (Compute ID ALUConnect {_left = Nothing, _right = RegA, _output = Just RegY} True, undefinedAddressingMode)
  -- General ALU operations: ORA, AND, EOR, ADC, LDA, CMP, SBC.
  $(bitPattern "......01") -> (Compute aluOp ALUConnect {_left = Just RegA, _right = Memory, _output = aluDest} True, aluAddressing)
  -- BIT.
  $(bitPattern "00100100") -> (Compute BIT ALUConnect {_left = Just RegA, _right = Memory, _output = Nothing} True, ZeroPage None)
  $(bitPattern "00101100") -> (Compute BIT ALUConnect {_left = Just RegA, _right = Memory, _output = Nothing} True, Absolute None)
  -- CPX, CPY.
  $(bitPattern "11.00.00") -> (Compute CMP ALUConnect {_left = Just cmpReg, _right = Memory, _output = Nothing} True, cmpAddressing)
  $(bitPattern "11.01100") -> (Compute CMP ALUConnect {_left = Just cmpReg, _right = Memory, _output = Nothing} True, Absolute None)
  -- ASL, ROL, LSR, ROR.
  $(bitPattern "0....110") -> (Compute shiftOp ALUConnect {_left = Nothing, _right = Memory, _output = Just Memory} True, shiftAddressing)
  $(bitPattern "0...1010") -> (Compute shiftOp ALUConnect {_left = Nothing, _right = RegA, _output = Just RegA} True, undefinedAddressingMode)
  -- DEX, INX
  $(bitPattern "11001010") -> (Compute SUB ALUConnect {_left = Just RegX, _right = One, _output = Just RegX} True, undefinedAddressingMode)
  $(bitPattern "11101000") -> (Compute ADD ALUConnect {_left = Just RegX, _right = One, _output = Just RegX} True, undefinedAddressingMode)
  -- DEY, INY
  $(bitPattern "10001000") -> (Compute SUB ALUConnect {_left = Just RegY, _right = One, _output = Just RegY} True, undefinedAddressingMode)
  $(bitPattern "11001000") -> (Compute ADD ALUConnect {_left = Just RegY, _right = One, _output = Just RegY} True, undefinedAddressingMode)
  -- INC, DEC.
  $(bitPattern "11...110") -> (Compute incDecOp ALUConnect {_left = Just Memory, _right = One, _output = Just Memory} True, incDecAddressing)
  -- LDX, LDY.
  $(bitPattern "101000.0") -> (Compute ID ALUConnect {_left = Nothing, _right = Memory, _output = loadTarget} True, Immediate)
  $(bitPattern "101..1.0") -> (Compute ID ALUConnect {_left = Nothing, _right = Memory, _output = loadTarget} True, loadAddressing)
  _ -> errorX "Unknown instruction"
  where
    jumpAddressing = if testBit op 5 then Indirect None else Absolute None

    branch = decodeBranch $ slice d7 d5 op

    storeAddressing reg = decodeStoreAddressing reg $ slice d4 d3 op

    aluOp = decodeAluOp $ slice d7 d5 op
    aluAddressing = decodeAluAddressing $ slice d4 d2 op
    aluDest = case aluOp of
      CMP -> Nothing
      _ -> Just RegA

    cmpAddressing = if testBit op 2 then ZeroPage None else Immediate
    cmpReg = if testBit op 5 then RegX else RegY

    shiftAddressing = aluAddressing
    shiftOp = ShiftOp . decodeShiftOp $ slice d6 d5 op

    incDecOp = if testBit op 5 then ADD else SUB
    incDecAddressing = decodeIncDecAddressing incDecMemTarget $ slice d4 d3 op
    incDecMemTarget = if testBit op 1 then YRegOffset else XRegOffset

    loadTarget = Just if testBit op 1 then RegX else RegY
    loadAddressing = decodeLoadAddressing $ slice d4 d3 op
