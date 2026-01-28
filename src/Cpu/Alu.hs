module Cpu.Alu where

import Clash.Prelude
import Cpu.Cpu
import Utilities.Utils

data ArithmeticFlags = ArithmeticFlags
  { negative :: Active High,
    overflow :: Active High,
    decimal :: Active High,
    zero :: Active High,
    carry :: Active High
  }
  deriving (Eq, Show, Generic, NFDataX)

defaultArithmeticFlags :: ArithmeticFlags
defaultArithmeticFlags =
  ArithmeticFlags
    { negative = toActive False,
      overflow = toActive False,
      decimal = toActive False,
      zero = toActive False,
      carry = toActive False
    }

data ALUBinaryOp = OR | AND | XOR
  deriving (Show, Eq, Generic, NFDataX)

data ALUShiftOp = ROR | ROL | LSR | ASL
  deriving (Show, Eq, Generic, NFDataX)

data ALU
  = BinaryOp ALUBinaryOp
  | ShiftOp ALUShiftOp
  | -- | Add (with carry).
    ALU_ADD Bool
  | -- | Subtract (with carry).
    ALU_SUB Bool
  | -- | Identity function applied to Register value. Updates CPU flags.
    ID
  deriving (Show, Eq, Generic, NFDataX)

pattern ADC, SBC, ADD, SUB, CMP :: ALU
pattern ADC = ALU_ADD True
pattern SBC = ALU_SUB True
pattern ADD = ALU_ADD False
pattern SUB = ALU_SUB False
pattern CMP = ALU_SUB False

alu :: ALU -> ArithmeticFlags -> Data -> Data -> (Data, ArithmeticFlags)
alu op flags x y = (result, newFlags)
  where
    carryFlag = fromActive (carry flags)
    bcdFlag = fromActive (decimal flags)

    -- Carry input number value for addition and subtraction.
    -- For addition 'carryFlag' is used on 'ADC' operation.
    -- For subtraction *by default* use 'carryInVal = 1', unless performing
    -- 'SBC' where behaviour is the same as for 'ADC'.
    carryInVal :: Unsigned 9
    carryInVal = bitCoerce (0 :: Unsigned 8, carryBit)
      where
        carryBit = case op of
          ALU_ADD c -> carryFlag && c
          ALU_SUB True -> carryFlag
          ALU_SUB False -> True
          _ -> False

    -- Use U2 properties (x - y = x + ~y + 1) for subtraction.
    addOperandY = case op of
      ALU_SUB _ -> complement y
      _ -> y

    extendX, extendY, extendSumXY :: Unsigned 9
    extendX = zeroExtend x
    extendY = zeroExtend addOperandY
    extendSumXY = extendX + extendY + carryInVal

    sumXY :: Data
    sumXY = truncateB extendSumXY

    signedOverflow = (xBit == yBit) && (sumBit /= xBit)
      where
        xBit = testBit x 7
        yBit = testBit addOperandY 7
        sumBit = testBit sumXY 7

    lowBit = slice d0 d0 y == 1
    middleBits = slice d6 d1 y
    highBit = slice d7 d7 y == 1

    -- BCD Handling Helpers.
    halfCarry = testBit (extendX `xor` extendY `xor` extendSumXY) 4
    binaryCarry = testBit extendSumXY 8

    adcLoAdjust = (sumXY .&. 0x0F) > 9 || halfCarry
    adcHiAdjust = sumXY > 0x99 || binaryCarry
    -- Add 6 to each nibble that either overflowed or is above 9.
    -- This guarantees to correct the result back into BCD range.
    adcCorrection = (if adcLoAdjust then 0x06 else 0) .|. (if adcHiAdjust then 0x60 else 0)

    sbcLoAdjust = (sumXY .&. 0x0F) > 9 || not halfCarry
    sbcHiAdjust = sumXY > 0x99 || not binaryCarry
    -- Add 6 to each nibble that either overflowed (by borrowing!) or is above 9.
    -- This guarantees to correct the result back into BCD range.
    sbcCorrection = (if sbcLoAdjust then 0x06 else 0) .|. (if sbcHiAdjust then 0x60 else 0)

    (result, newFlags) = case op of
      ALU_ADD _ ->
        let res = if bcdFlag then sumXY + adcCorrection else sumXY
            v = toActive signedOverflow
            c = toActive $ if bcdFlag then adcHiAdjust else binaryCarry
         in (res, flagsNZ {overflow = v, carry = c})
      ALU_SUB _ ->
        let res = if bcdFlag then sumXY - sbcCorrection else sumXY
            v = toActive signedOverflow
            c = toActive $ if bcdFlag then not sbcHiAdjust else binaryCarry
         in (res, flagsNZ {overflow = v, carry = c})
      BinaryOp binOp ->
        let res = case binOp of
              OR -> x .|. y
              AND -> x .&. y
              XOR -> x `xor` y
         in (res, flagsNZ)
      ShiftOp shiftOp ->
        let (res, shiftCarry) = case shiftOp of
              ROR -> (bitCoerce (lowBit, highBit, middleBits), lowBit)
              ROL -> (bitCoerce (middleBits, lowBit, highBit), highBit)
              LSR -> (bitCoerce (False, highBit, middleBits), lowBit)
              ASL -> (bitCoerce (middleBits, lowBit, False), highBit)
         in (res, flagsNZ {carry = toActive shiftCarry})
      ID -> (y, flagsNZ)

    zeroFlag = toActive (result == 0)
    negativeFlag = toActive (testBit result 7)
    flagsNZ =
      flags
        { negative = negativeFlag,
          zero = zeroFlag
        }
