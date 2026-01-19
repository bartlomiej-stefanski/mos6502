module Cpu.Microcode where

import Clash.Prelude

data MicroCmd
  = -- | Execute actual instruction.
    CmdExecute
  | -- | Decode opcode on bus to uncover next MicroOP.
    CmdDecodeOpcode
  | -- | Do not perform operations.
    CmdNOP
  deriving (Eq, Show)

data BusAddress = PC | SP | COMPUTE_FROM_MODE
  deriving (Eq, Show)

data BusDataSource
  = DATA_SOURCE_PC_LOW
  | DATA_SOURCE_PC_HIGH
  | DATA_SOURCE_STATUS
  | DATA_SOURCE_ALU
  deriving (Eq, Show)

data BusOP
  = BusOP
  { -- | Some address must be specified; even if result will not be used.
    address :: BusAddress,
    -- | Data to write specified address. If empty treat as read.
    writeData :: Maybe BusDataSource
  }
  deriving (Eq, Show)

data MicroOP
  = MicroOP
  { cmd :: MicroCmd,
    busOp :: BusOP
  }
  deriving (Eq, Show)
