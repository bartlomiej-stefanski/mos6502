module Cpu.CpuState where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.Instructions
import Utilities.Utils

data CpuFlags = CpuFlags
  { arithmeticFlags :: ArithmeticFlags,
    brk :: Active High,
    interrupt :: Active High
  }
  deriving (Eq, Show, Generic, NFDataX)

defaultCpuFlags :: CpuFlags
defaultCpuFlags =
  CpuFlags
    { arithmeticFlags = defaultArithmeticFlags,
      brk = toActive False,
      interrupt = toActive False
    }

data CpuState
  = CpuState
  { regA, regX, regY :: Data,
    regSP :: Data,
    regPC :: Addr,
    cpuFlags :: CpuFlags,
    dataLatch :: Data,
    -- | Instruction currently being executed.
    instruction :: Instruction,
    -- | Addressing mode for current instruction.
    addressingMode :: AddressingMode
    -- TODO: Include microcode ROM address.
  }
  deriving (Eq, Show, Generic, NFDataX)

initCpuState :: CpuState
initCpuState =
  CpuState
    { regA = 0,
      regX = 0,
      regY = 0,
      regSP = 0xFF,
      regPC = 0x0000,
      cpuFlags = defaultCpuFlags,
      dataLatch = 0,
      instruction = NOP,
      addressingMode = Implied
    }

stackOffset :: Addr
stackOffset = 0x100
