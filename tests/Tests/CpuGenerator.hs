module Tests.CpuGenerator where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.CpuState
import Cpu.Instructions
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
genOpcode = do
  Gen.choice $ Prelude.map return instructions
  where
    lowNibble = [0 .. 7] :: [Unsigned 4]
    highNibble = [8 .. 15] :: [Unsigned 4]
    nibble = lowNibble Prelude.++ highNibble

    nibbleConnector h l = bitCoerce (h, l) :: Data

    horizontalStripes = nibbleConnector <$> nibble <*> stripes
      where
        stripes = [1, 5, 6, 8, 13] :: [Unsigned 4]

    lowStripes = nibbleConnector <$> lowNibble <*> stripes
      where
        stripes = [0, 9, 14] :: [Unsigned 4]

    highStripes = nibbleConnector <$> almostHighNibble <*> stripes
      where
        almostHighNibble = [9 .. 15] :: [Unsigned 4]
        stripes = [0, 9] :: [Unsigned 4]

    ldxImmediate = [0xa2] :: [Data]
    bitZeropage = [0x24] :: [Data]
    compareWithX = [0xe4] :: [Data]
    storeLoadZeroPage = nibbleConnector <$> row <*> [4 :: Unsigned 4]
      where
        row = [8 .. 12] :: [Unsigned 4]

    aslAbsolute = [0x0e] :: [Data]
    transferXImplied = [0x9a] :: [Data]
    transferLoad = nibbleConnector <$> row <*> stripe
      where
        row = [2, 4, 6, 8, 10, 11, 12, 14] :: [Unsigned 4]
        stripe = [10, 12] :: [Unsigned 4]

    incDecAbsolute = nibbleConnector <$> row <*> [14 :: Unsigned 4]
      where
        row = [10 .. 15] :: [Unsigned 4]

    instructions =
      horizontalStripes
        Prelude.++ lowStripes
        Prelude.++ highStripes
        Prelude.++ ldxImmediate
        Prelude.++ bitZeropage
        Prelude.++ compareWithX
        Prelude.++ storeLoadZeroPage
        Prelude.++ aslAbsolute
        Prelude.++ transferXImplied
        Prelude.++ transferLoad
        Prelude.++ incDecAbsolute
