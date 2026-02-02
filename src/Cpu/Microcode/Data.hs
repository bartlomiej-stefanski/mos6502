module Cpu.Microcode.Data where

import Clash.Prelude

data MicroCmd
  = -- | Execute actual instruction.
    CmdExecute
  | -- | Decode opcode on bus to uncover next MicroOP.
    -- This is the last instruction in a chain.
    CmdDecodeOpcode
  | -- | Do not perform operations.
    CmdNOP
  deriving (Eq, Show, Generic, NFDataX, Lift)

data BusAddressOffset = NONE | REGX | REGY
  deriving (Eq, Show, Generic, NFDataX, Lift)

data BusAddress
  = SP
  | SP_INC
  | PC
  | BUS_VALUE
  | DATA_LATCH_AND_BUS
  | LAST_BUS_ADDRESS
  | LAST_BUS_ADDRESS_PLUS_ONE
  deriving (Eq, Show, Generic, NFDataX, Lift)

data BusDataSourceWrite
  = DATA_WRITE_PC_LOW
  | DATA_WRITE_PC_HIGH
  | DATA_WRITE_STATUS
  | DATA_WRITE_ALU
  deriving (Eq, Show, Generic, NFDataX, Lift)

data BusDataSourceRead
  = DATA_READ_PC
  | DATA_READ_STATUS
  | DATA_READ
  deriving (Eq, Show, Generic, NFDataX, Lift)

data BusOP
  = BusOP
  { -- | Optional address and offset for this operation.
    -- Lack of new data will preserve current bus address.
    _address :: Maybe (BusAddress, BusAddressOffset),
    -- | Data to write onto bus. If empty treat as read request.
    _writeData :: Maybe BusDataSourceWrite,
    -- | What happens with data on bus.
    _readData :: Maybe BusDataSourceRead
  }
  deriving (Eq, Show, Generic, NFDataX, Lift)

data SPChange = SPIncrement | SPDecrement | SPNone
  deriving (Eq, Show, Generic, NFDataX, Lift)

data MicroOP
  = MicroOP
  { _cmd :: MicroCmd,
    _busOp :: BusOP,
    _incrementPC :: Bool,
    _spOperation :: SPChange
  }
  deriving (Eq, Show, Generic, NFDataX, Lift)

nopBusOP :: BusOP
nopBusOP =
  BusOP
    { _address = Nothing,
      _writeData = Nothing,
      _readData = Nothing
    }

nopMicroOP :: MicroOP
nopMicroOP =
  MicroOP
    { _cmd = CmdNOP,
      _busOp = nopBusOP,
      _incrementPC = False,
      _spOperation = SPNone
    }
