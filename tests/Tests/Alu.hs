module Tests.Alu where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.Instructions
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Utilities.Utils
import qualified Prelude

valueToData :: Integer -> Data
valueToData = fromIntegral . (.&. 0xFF)

bitOps :: [ALU]
bitOps =
  [ BinaryOp AND,
    BinaryOp OR,
    BinaryOp XOR,
    ID,
    ShiftOp ROR,
    ShiftOp ROL,
    ShiftOp LSR,
    ShiftOp ASL
  ]

aluOps :: [ALU]
aluOps = bitOps Prelude.++ [ADC, ADD, SBC, SUB]

prop_alu_addition :: H.Property
prop_alu_addition = H.property do
  x <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  y <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  op <- H.forAll $ Gen.choice [return ADC, return ADD]
  carryFlag <- H.forAll Gen.bool

  let carryAddition = case op of
        ADC -> if carryFlag then 1 else 0
        _ -> 0

  let addResult = x + y + carryAddition
  let addResultData = fromIntegral addResult :: Data

  let highXBit = x >= 128
  let highYBit = y >= 128
  let highResultBit = addResultData >= 128

  let resOverflowFlag = (highXBit == highYBit) && (highResultBit /= highXBit)
  let resZeroFlag = addResultData == 0
  let resNegativeFlag = highResultBit
  let resCarryFlag = addResult >= 256

  let adding_alu = alu op $ defaultArithmeticFlags {carry = toActive carryFlag}
  let (result, flags) = adding_alu (valueToData x) (valueToData y)
  result H.=== addResultData
  (fromActive . overflow $ flags) H.=== resOverflowFlag
  (fromActive . zero $ flags) H.=== resZeroFlag
  (fromActive . negative $ flags) H.=== resNegativeFlag
  (fromActive . carry $ flags) H.=== resCarryFlag

prop_alu_subtraction :: H.Property
prop_alu_subtraction = H.property do
  x <- H.forAll $ Gen.integral $ Range.linear (-128 :: Integer) 127
  y <- H.forAll $ Gen.integral $ Range.linear (-128 :: Integer) 127
  op <- H.forAll $ Gen.choice $ Prelude.map return [SBC, SUB]
  carryFlag <- H.forAll Gen.bool

  let carrySubtract = case op of
        SBC -> if carryFlag then 0 else 1
        _ -> 0

  let yOperand = y + carrySubtract
  let subResult = x - yOperand
  let subResultData = fromIntegral subResult :: Data

  let highXBit = x < 0
  -- Highest bit of 'y' operand is high when complement (offset by carry)
  -- is negative (that is - the original value is positive).
  let highYBit = yOperand > 0
  let highResultBit = subResultData < 0 || subResultData >= 128

  let resOverflowFlag = (highXBit == highYBit) && (highResultBit /= highXBit)
  let resZeroFlag = (subResult .&. 0xFF) == 0
  let resNegativeFlag = highResultBit

  let xExtend = fromInteger $ x .&. 0xFF :: Unsigned 9
  let yExtend = fromInteger $ y .&. 0xFF :: Unsigned 9

  -- In MOS 6502 the carry flag is set to '1' on subtraction when operation did not need a borrow.
  let resCarryFlag = xExtend >= (yExtend + fromInteger carrySubtract)

  let sub_alu = alu op $ defaultArithmeticFlags {carry = toActive carryFlag}
  let (result, flags) = sub_alu (valueToData x) (valueToData y)
  result H.=== subResultData
  (fromActive . overflow $ flags) H.=== resOverflowFlag
  (fromActive . zero $ flags) H.=== resZeroFlag
  (fromActive . negative $ flags) H.=== resNegativeFlag
  (fromActive . carry $ flags) H.=== resCarryFlag

setNZ :: ArithmeticFlags -> Data -> ArithmeticFlags
setNZ flags res =
  flags
    { negative = toActive (testBit res 7),
      zero = toActive (res == 0)
    }

nzOpTest :: ALU -> (forall a. (Bits a) => a -> a -> a) -> H.Property
nzOpTest op opFunc = H.property do
  x <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  y <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  -- These operations should not affect the carry flag.
  carryFlag <- H.forAll Gen.bool
  let arithmeticFlagsCarry = defaultArithmeticFlags {carry = toActive carryFlag}
  let opAlu = alu op arithmeticFlagsCarry
  let (result, flags) = opAlu (valueToData x) (valueToData y)
  let expectedResult = fromIntegral $ opFunc x y :: Data
  result H.=== expectedResult
  flags H.=== setNZ arithmeticFlagsCarry expectedResult

prop_alu_and :: H.Property
prop_alu_and = nzOpTest (BinaryOp AND) (.&.)

prop_alu_or :: H.Property
prop_alu_or = nzOpTest (BinaryOp OR) (.|.)

prop_alu_xor :: H.Property
prop_alu_xor = nzOpTest (BinaryOp XOR) xor

prop_alu_id :: H.Property
prop_alu_id = nzOpTest ID $ \_ y -> y

shiftOpTest :: ALUShiftOp -> (Data -> (Data, Bool)) -> H.Property
shiftOpTest shiftOp opFunc = H.property do
  x <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  y <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  let shiftAlu = alu (ShiftOp shiftOp) defaultArithmeticFlags
  let (result, flags) = shiftAlu (valueToData x) (valueToData y)
  let (expectedResult, carryFlag) = opFunc (fromIntegral y :: Data)
  let expectedFlagsCarry = defaultArithmeticFlags {carry = toActive carryFlag}
  result H.=== expectedResult
  flags H.=== setNZ expectedFlagsCarry expectedResult

prop_alu_ror :: H.Property
prop_alu_ror = shiftOpTest ROR
  $ \y -> (rotateR y 1, testBit y 0)

prop_alu_rol :: H.Property
prop_alu_rol = shiftOpTest ROL
  $ \y -> (rotateL y 1, testBit y 7)

prop_alu_lsr :: H.Property
prop_alu_lsr = shiftOpTest LSR
  $ \y -> (shiftR y 1, testBit y 0)

prop_alu_asl :: H.Property
prop_alu_asl = shiftOpTest ASL
  $ \y -> (shiftL y 1, testBit y 7)

prop_alu_ops_do_not_change_decimal_flag :: H.Property
prop_alu_ops_do_not_change_decimal_flag = H.property do
  x <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  y <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  op <- H.forAll $ Gen.choice $ Prelude.map return aluOps
  decimalFlag <- H.forAll Gen.bool
  let aluOp = alu op $ defaultArithmeticFlags {decimal = toActive decimalFlag}
  let (_, flags) = aluOp (valueToData x) (valueToData y)
  (fromActive . decimal $ flags) H.=== decimalFlag

prop_alu_binary_ops_do_not_change_overflow_flag :: H.Property
prop_alu_binary_ops_do_not_change_overflow_flag = H.property do
  x <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  y <- H.forAll $ Gen.integral $ Range.linear (0 :: Integer) 255
  op <- H.forAll $ Gen.choice $ Prelude.map return bitOps
  overflowFlag <- H.forAll Gen.bool
  let aluOp = alu op $ defaultArithmeticFlags {overflow = toActive overflowFlag}
  let (_, flags) = aluOp (valueToData x) (valueToData y)
  (fromActive . overflow $ flags) H.=== overflowFlag

aluTests :: TestTree
aluTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain aluTests
