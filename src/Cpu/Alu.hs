module Cpu.Alu where

import Clash.Prelude

import Utilities.Utils
import Cpu.Instructions
import Cpu.Cpu


-- TODO: Implement BCD flag handling.
alu :: ALU -> ArithmeticFlags -> Data -> Data -> (Data, ArithmeticFlags)
alu op flags x y = (result, newFlags)
  where
    carryFlag = fromActive (carry flags)

    -- | Carry input number value for addition and subtraction.
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
      _         -> y

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

    (result, newFlags) = case op of
      ALU_ADD _ ->
        let
          res = sumXY
          v = toActive signedOverflow
          c = toActive $ testBit extendSumXY 8
        in (res, flagsNZ{ overflow = v, carry = c })
      ALU_SUB _ ->
        let
          res = sumXY
          v = toActive signedOverflow
          c = toActive $ testBit extendSumXY 8
        in (res, flagsNZ{ overflow = v, carry = c })
      BinaryOp binOp ->
        let
          res = case binOp of
            OR  -> x .|. y
            AND -> x .&. y
            XOR -> x `xor` y
        in (res, flagsNZ)
      ShiftOp shiftOp ->
        let
          (res, shiftCarry) = case shiftOp of
            ROR -> (bitCoerce (lowBit, highBit, middleBits), lowBit)
            ROL -> (bitCoerce (middleBits, lowBit, highBit), highBit)
            LSR -> (bitCoerce (False, highBit, middleBits), lowBit)
            ASL -> (bitCoerce (middleBits, lowBit, False), highBit)
        in (res, flagsNZ{ carry = toActive shiftCarry })
      ID -> (y, flagsNZ)

    zeroFlag = toActive (result == 0)
    negativeFlag = toActive (testBit result 7)
    flagsNZ = flags
      { negative = negativeFlag
      , zero     = zeroFlag
      }
