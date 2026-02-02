module Tests.CpuGenerator where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.CpuState
import Cpu.Instructions
import Cpu.Microcode.Map
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Utilities.Utils
import qualified Prelude

-- | Generates a random ArithmeticFlags for use in property tests.
genArithmeticFlags :: H.Gen ArithmeticFlags
genArithmeticFlags = do
  n <- Gen.bool
  v <- Gen.bool
  dec <- Gen.bool
  z <- Gen.bool
  c <- Gen.bool

  return
    $ ArithmeticFlags
      { _negative = toActive n,
        _overflow = toActive v,
        _decimal = toActive dec,
        _zero = toActive z,
        _carry = toActive c
      }

-- | Generates a random CpuFlags for use in property tests.
genCpuFlags :: H.Gen CpuFlags
genCpuFlags = do
  brk <- Gen.bool
  interrupt <- Gen.bool

  arithmeticFlags <- genArithmeticFlags

  return
    $ CpuFlags
      { _arithmeticFlags = arithmeticFlags,
        _brk = toActive brk,
        _interrupt = toActive interrupt
      }

-- | Generates a random CpuState for use in property tests.
-- Note that 'instruction=NOP' and 'addressingMode=Impled'.
genCpuState :: H.Gen CpuState
genCpuState = do
  a <- Gen.word8 Range.linearBounded
  x <- Gen.word8 Range.linearBounded
  y <- Gen.word8 Range.linearBounded
  pc <- Gen.word16 Range.linearBounded
  sp <- Gen.word8 Range.linearBounded

  flags <- genCpuFlags

  return
    $ CpuState
      { _regA = bitCoerce a,
        _regX = bitCoerce x,
        _regY = bitCoerce y,
        _regPC = bitCoerce pc,
        _regSP = bitCoerce sp,
        _cpuFlags = flags,
        _dataLatch = 0,
        _instruction = NOP
      }

genData :: H.Gen Data
genData = do
  value <- Gen.word8 Range.linearBounded
  return $ bitCoerce value

genAddr :: H.Gen Addr
genAddr = do
  value <- Gen.word16 Range.linearBounded
  return $ bitCoerce value

genAluOp :: H.Gen ALU
genAluOp = do
  Gen.choice $ Prelude.map return [ADD, ADC, SBC, SUB, BinaryOp AND, BinaryOp OR, BinaryOp XOR, ID, ShiftOp ASL, ShiftOp LSR, ShiftOp ROL, ShiftOp ROR, CMP, BIT]

genOpcode :: H.Gen Data
genOpcode = Gen.choice $ Prelude.map return opcodeList
