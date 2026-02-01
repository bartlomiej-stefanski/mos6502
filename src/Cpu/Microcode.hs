module Cpu.Microcode where

import Clash.Prelude
import Cpu.Instructions
import qualified Prelude

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
  | SP_INC
  | PC
  | BUS_VALUE
  | DATA_LATCH_AND_BUS
  | LAST_BUS_ADDRESS
  | LAST_BUS_ADDRESS_PLUS_ONE
  deriving (Eq, Show, Generic, NFDataX)

data BusDataSourceWrite
  = DATA_WRITE_PC_LOW
  | DATA_WRITE_PC_HIGH
  | DATA_WRITE_STATUS
  | DATA_WRITE_ALU
  deriving (Eq, Show, Generic, NFDataX)

data BusDataSourceRead
  = DATA_READ_PC
  | DATA_READ_STATUS
  | DATA_READ
  deriving (Eq, Show, Generic, NFDataX)

data BusOP
  = BusOP
  { -- | Some address must be specified; even if result will not be used.
    _address :: Maybe (BusAddress, BusAddressOffset),
    -- | Data to write onto bus. If empty treat as read request.
    _writeData :: Maybe BusDataSourceWrite,
    -- | What happens with data on bus.
    _readData :: Maybe BusDataSourceRead
  }
  deriving (Eq, Show, Generic, NFDataX)

data SPChange = SPIncrement | SPDecrement | SPNone
  deriving (Eq, Show, Generic, NFDataX)

data MicroOP
  = MicroOP
  { _cmd :: MicroCmd,
    _busOp :: BusOP,
    _incrementPC :: Bool,
    _spOperation :: SPChange
  }
  deriving (Eq, Show, Generic, NFDataX)

type MicroOpIndex = Index 1024

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

getNextMicrocodeIndex :: (Instruction, AddressingMode) -> MicroOpIndex
getNextMicrocodeIndex = errorX "Not implemented"

addressOffsetToBusAddressOffset :: AddressOffset -> BusAddressOffset
addressOffsetToBusAddressOffset offset = case offset of
  None -> NONE
  XRegOffset -> REGX
  YRegOffset -> REGY

executeCmd :: MicroOP -> MicroOP
executeCmd microOP = microOP {_cmd = CmdExecute}

readFromBus :: BusDataSourceRead -> MicroOP -> MicroOP
readFromBus dataSource microOP =
  microOP {_busOp = busOp {_readData = Just dataSource}}
  where
    busOp = _busOp microOP

writeToBus :: BusAddress -> BusAddressOffset -> BusDataSourceWrite -> MicroOP -> MicroOP
writeToBus address addressOffset dataSource microOP =
  microOP
    { _busOp =
        busOp
          { _address = Just (address, addressOffset),
            _writeData = Just dataSource
          }
    }
  where
    busOp = _busOp microOP

placeDataOnBus :: BusAddress -> BusAddressOffset -> MicroOP -> MicroOP
placeDataOnBus address addressOffset microOP =
  microOP {_busOp = busOp {_address = Just (address, addressOffset)}}
  where
    busOp = _busOp microOP

microOPIncrementPC :: MicroOP -> MicroOP
microOPIncrementPC microOP = microOP {_incrementPC = True}

microOPChangeSP :: SPChange -> MicroOP -> MicroOP
microOPChangeSP spChange microOP = microOP {_spOperation = spChange}

microcodeGenerator :: (Instruction, AddressingMode) -> [MicroOP -> MicroOP]
microcodeGenerator (instruction, addressingMode) =
  case (instruction, addressingMode) of
    (NOP, _) -> [placeNextOpcodeOnBus]
    (PHP, _) -> [pushToStack DATA_WRITE_STATUS, placeNextOpcodeOnBus]
    (PLP, _) -> [popFromStack, readFromBus DATA_READ_STATUS, placeNextOpcodeOnBus]
    (SEC, _) -> [executeCmd . placeNextOpcodeOnBus]
    (CLC, _) -> [executeCmd . placeNextOpcodeOnBus]
    (SED, _) -> [executeCmd . placeNextOpcodeOnBus]
    (CLD, _) -> [executeCmd . placeNextOpcodeOnBus]
    (CLV, _) -> [executeCmd . placeNextOpcodeOnBus]
    (SEI, _) -> [executeCmd . placeNextOpcodeOnBus]
    (CLI, _) -> [executeCmd . placeNextOpcodeOnBus]
    -- Note that JSR saves 'nextPC - 1' onto stack by design! RTS will add the missing 1.
    (JSR, _) ->
      [ placeImmediatOnBus,
        -- Currently PCLow is on bus, latch it for later.
        pushToStack DATA_WRITE_PC_HIGH . readFromBus DATA_READ,
        pushToStack DATA_WRITE_PC_LOW,
        placeImmediatOnBus,
        -- At this point PCHigh is on bus, and PCLow is still latched.
        -- Set PC = (PCHigh, PCLow) and fetch next opcode.
        microOPIncrementPC . readFromBus DATA_READ_PC . placeDataOnBus DATA_LATCH_AND_BUS NONE
      ]
    (RTS, _) ->
      [ popFromStack,
        popFromStack . readFromBus DATA_READ,
        -- Increment PC one time to get next instruction.
        microOPIncrementPC . readFromBus DATA_READ_PC,
        placeNextOpcodeOnBus
      ]
    -- BRK instruction not implemented!
    (BRK, _) -> [placeNextOpcodeOnBus]
    -- RTI instruction not implemented!
    (RTI, _) -> [placeNextOpcodeOnBus]
    (JMP, _) ->
      -- After 'loadToBus' the PCLow is available on bus
      loadToBus id
        Prelude.++ [ readFromBus DATA_READ . placeDataOnBus LAST_BUS_ADDRESS_PLUS_ONE NONE,
                     microOPIncrementPC . readFromBus DATA_READ_PC . placeDataOnBus DATA_LATCH_AND_BUS NONE
                   ]
    -- TODO: Fetch next opcode on the same cycle the branch is taken.
    (BRANCH _, _) -> [placeImmediatOnBus, executeCmd, placeNextOpcodeOnBus]
    (Compute _ (ALUConnect left right output) _, _) ->
      case (usesLoadedData, storesToMemory) of
        (False, False) -> [executeCmd . placeNextOpcodeOnBus]
        (True, False) -> loadToBus id Prelude.++ [executeCmd . placeNextOpcodeOnBus]
        (False, True) -> loadToBus (writeAluResult . executeCmd) Prelude.++ [placeNextOpcodeOnBus]
        (True, True) -> loadToBus id Prelude.++ [writeToBus LAST_BUS_ADDRESS NONE DATA_WRITE_ALU . executeCmd, placeNextOpcodeOnBus]
      where
        usesLoadedData = case (left, right) of
          (Just Memory, _) -> True
          (_, Memory) -> True
          _ -> False
        storesToMemory = output == Just Memory
  where
    placeNextOpcodeOnBus = microOPIncrementPC . placeDataOnBus PC NONE
    placeImmediatOnBus = microOPIncrementPC . placeDataOnBus PC NONE

    writeAluResult microOP = microOP {_busOp = busOp {_writeData = Just DATA_WRITE_ALU}}
      where
        busOp = _busOp microOP

    pushToStack dataSource = microOPChangeSP SPDecrement . writeToBus SP NONE dataSource
    popFromStack = microOPChangeSP SPIncrement . placeDataOnBus SP_INC NONE

    -- \| For a given addressing mode returns a sequence of micro-operations that will
    -- end with the requested address present on the bus.
    loadToBus :: (MicroOP -> MicroOP) -> [MicroOP -> MicroOP]
    loadToBus lastMicroOP = case addressingMode of
      Immediate -> [placeImmediatOnBus]
      Absolute offset ->
        [ placeImmediatOnBus,
          placeImmediatOnBus . readFromBus DATA_READ,
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS (addressOffsetToBusAddressOffset offset)
        ]
      ZeroPage offset ->
        [ placeImmediatOnBus,
          lastMicroOP . placeDataOnBus BUS_VALUE (addressOffsetToBusAddressOffset offset)
        ]
      Indirect None ->
        [ placeImmediatOnBus,
          placeImmediatOnBus . readFromBus DATA_READ,
          -- Load the low-byte of the addreess, requested address will be latched on bus.
          placeDataOnBus DATA_LATCH_AND_BUS NONE,
          -- Load the high-byte of the address, requested address will be latched on bus.
          placeDataOnBus LAST_BUS_ADDRESS_PLUS_ONE NONE,
          -- Now we have low-byte latched and high-byte on bus, request the actual data.
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS NONE
        ]
      Indirect XRegOffset ->
        [ placeImmediatOnBus,
          placeImmediatOnBus . readFromBus DATA_READ,
          -- Load the low-byte of the addreess, requested address will be latched on bus.
          -- Use the value in X register as offset pre-indexing.
          placeDataOnBus DATA_LATCH_AND_BUS REGX,
          -- Load the high-byte of the address, requested address will be latched on bus.
          placeDataOnBus LAST_BUS_ADDRESS_PLUS_ONE NONE,
          -- Now we have low-byte latched and high-byte on bus, request the actual data.
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS NONE
        ]
      Indirect YRegOffset ->
        [ placeImmediatOnBus,
          placeImmediatOnBus . readFromBus DATA_READ,
          -- Load the low-byte of the addreess, requested address will be latched on bus.
          placeDataOnBus DATA_LATCH_AND_BUS NONE,
          -- Load the high-byte of the address, requested address will be latched on bus.
          placeDataOnBus LAST_BUS_ADDRESS_PLUS_ONE NONE,
          -- Now we have low-byte latched and high-byte on bus, request the actual data.
          -- Use the value in Y register as offset post-indexing.
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS REGY
        ]
      StackPointer ->
        [ \microOP ->
            let op = lastMicroOP microOP
             in case _writeData $ _busOp op of
                  Nothing -> popFromStack op
                  Just writeData -> pushToStack writeData op
        ]
