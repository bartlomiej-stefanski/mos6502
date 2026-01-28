module Cpu.Microcode where

import Clash.Prelude

data MicroCmd
  = -- | Execute actual instruction.
    CmdExecute
  | -- | Decode opcode on bus to uncover next MicroOP.
    -- This is the last instructino in a chain.
    CmdDecodeOpcode
  | -- | Do not perform operations.
    CmdNOP
  deriving (Eq, Show, Generic, NFDataX)

data BusAddress = PC | SP | COMPUTE_FROM_MODE
  deriving (Eq, Show, Generic, NFDataX)

data BusDataSourceWrite
  = DATA_WRITE_PC_LOW
  | DATA_WRITE_PC_HIGH
  | DATA_WRITE_STATUS
  | DATA_WRITE_ALU
  deriving (Eq, Show, Generic, NFDataX)


data BusDataSourceRead
  = DATA_READ_PC_LOW
  | DATA_READ_PC_HIGH
  | DATA_READ_STATUS
  | DATA_READ_POINTER_LOW
  deriving (Eq, Show, Generic, NFDataX)

data BusOP
  = BusOP
  { -- | Some address must be specified; even if result will not be used.
    address :: BusAddress,
    -- | Data to write onto bus. If empty treat as read request.
    writeData :: Maybe BusDataSourceWrite,
    -- | What happens with data on bus.
    readData :: Maybe BusDataSourceRead
  }
  deriving (Eq, Show, Generic, NFDataX)

data MicroOP
  = MicroOP
  { cmd :: MicroCmd,
    busOp :: BusOP
  }
  deriving (Eq, Show, Generic, NFDataX)
