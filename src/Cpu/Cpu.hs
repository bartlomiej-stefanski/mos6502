module Cpu.Cpu where

import Clash.Prelude
import Utilities.Utils

type Data = Unsigned 8

type Addr = Unsigned 16

data ArithmeticFlags = ArithmeticFlags
  { negative :: Active High,
    overflow :: Active High,
    decimal :: Active High,
    zero :: Active High,
    carry :: Active High
  }
  deriving (Eq, Show)

defaultArithmeticFlags :: ArithmeticFlags
defaultArithmeticFlags =
  ArithmeticFlags
    { negative = toActive False,
      overflow = toActive False,
      decimal = toActive False,
      zero = toActive False,
      carry = toActive False
    }

data CpuFlags = CpuFlags
  { arithmeticFlags :: ArithmeticFlags,
    brk :: Active High,
    interrupt :: Active High
  }

defaultCpuFlags :: CpuFlags
defaultCpuFlags =
  CpuFlags
    { arithmeticFlags = defaultArithmeticFlags,
      brk = toActive False,
      interrupt = toActive False
    }
