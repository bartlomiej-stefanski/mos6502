module Cpu.Microcode where

import Clash.Prelude

data MicroCmd
  = -- | Execute actual instruction.
    CmdExecute
  | -- | Decode opcode on bus to uncover next MicroOP.
    CmdDecodeOpcode
  | -- | Do not perform operations.
    CmdNOP
  deriving (Eq, Show, Generic, NFDataX)

data BusAddress = PC | SP | COMPUTE_FROM_MODE
  deriving (Eq, Show, Generic, NFDataX)

data BusDataSource
  = DATA_SOURCE_PC_LOW
  | DATA_SOURCE_PC_HIGH
  | DATA_SOURCE_STATUS
  | DATA_SOURCE_ALU
  deriving (Eq, Show, Generic, NFDataX)

data BusOP
  = BusOP
  { -- | Some address must be specified; even if result will not be used.
    _address :: BusAddress,
    -- | Data to write specified address. If empty treat as read.
    _writeData :: Maybe BusDataSource
  }
  deriving (Eq, Show, Generic, NFDataX)

data MicroOP
  = MicroOP
  { _cmd :: MicroCmd,
    _busOp :: BusOP
  }
  deriving (Eq, Show, Generic, NFDataX)
