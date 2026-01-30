module Cpu.CpuState where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.Instructions
import Utilities.Utils

data CpuFlags = CpuFlags
  { _arithmeticFlags :: ArithmeticFlags,
    _brk :: Active High,
    _interrupt :: Active High
  }
  deriving (Eq, Show, Generic, NFDataX)

defaultCpuFlags :: CpuFlags
defaultCpuFlags =
  CpuFlags
    { _arithmeticFlags = defaultArithmeticFlags,
      _brk = toActive False,
      _interrupt = toActive False
    }

-- | Packs the 'CpuFlags' structure into a single byte used by the MOS6502.
cpuFlagsToData :: CpuFlags -> Data
cpuFlagsToData flags =
  bitCoerce
    ( _negative aflags,
      _overflow aflags,
      0 :: Bit,
      _brk flags,
      _decimal aflags,
      _interrupt flags,
      _zero aflags,
      _carry aflags
    )
  where
    aflags = _arithmeticFlags flags

cpuFlagsFromData :: Data -> CpuFlags
cpuFlagsFromData d =
  CpuFlags
    { _arithmeticFlags = ArithmeticFlags n v dec z c,
      _brk = brk,
      _interrupt = interrupt
    }
  where
    (n, v, _ :: Bit, brk, dec, interrupt, z, c) = bitCoerce d

data CpuState
  = CpuState
  { _regA, _regX, _regY :: Data,
    _regSP :: Data,
    _regPC :: Addr,
    _cpuFlags :: CpuFlags,
    _dataLatch :: Data,
    -- | Instruction currently being executed.
    _instruction :: Instruction,
    -- | Addressing mode for current instruction.
    _addressingMode :: AddressingMode
    -- TODO: Include microcode ROM address.
  }
  deriving (Eq, Show, Generic, NFDataX)

initCpuState :: CpuState
initCpuState =
  CpuState
    { _regA = 0,
      _regX = 0,
      _regY = 0,
      _regSP = 0xFF,
      _regPC = 0x0000,
      _cpuFlags = defaultCpuFlags,
      _dataLatch = 0,
      _instruction = NOP,
      _addressingMode = Implied
    }
