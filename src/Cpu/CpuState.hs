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
