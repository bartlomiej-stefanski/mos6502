module Cpu.Microcode where

import Clash.Prelude
import Cpu.Instructions (AddressingMode, Instruction)

data MicroCmd
  = -- | Execute actual instruction.
    CmdExecute
  | -- | Decode opcode on bus to uncover next MicroOP.
    -- This is the last instruction in a chain.
    CmdDecodeOpcode
  | -- | Do not perform operations.
    CmdNOP
  deriving (Eq, Show, Generic, NFDataX)

data BusAddressOffset = NONE | REGX | REGY
  deriving (Eq, Show, Generic, NFDataX)

data BusAddress
  = SP
  | PC
  | BUS_VALUE
  | DATA_LATCH_AND_BUS
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
  | DATA_READ
  deriving (Eq, Show, Generic, NFDataX)

data BusOP
  = BusOP
  { -- | Some address must be specified; even if result will not be used.
    _address :: BusAddress,
    _addressOffset :: BusAddressOffset,
    -- | Data to write onto bus. If empty treat as read request.
    _writeData :: Maybe BusDataSourceWrite,
    -- | What happens with data on bus.
    _readData :: Maybe BusDataSourceRead
  }
  deriving (Eq, Show, Generic, NFDataX)

data MicroOP
  = MicroOP
  { _cmd :: MicroCmd,
    _busOp :: BusOP,
    _incrementPC :: Bool
  }
  deriving (Eq, Show, Generic, NFDataX)

type MicroOpIndex = Index 1024

getNextMicrocodeIndex :: (Instruction, AddressingMode) -> MicroOpIndex
getNextMicrocodeIndex = errorX "Not Implemented"
