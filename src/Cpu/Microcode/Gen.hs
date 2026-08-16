module Cpu.Microcode.Gen where

import Clash.Prelude
import Cpu.Instructions
import Cpu.Microcode.Data
import qualified Prelude

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

decodeAndFetchPC :: MicroOPSource -> MicroOP -> MicroOP
decodeAndFetchPC nextMicroOPSource microOp =
  microOPIncrementPC . placeDataOnBus PC NONE $ microOp {_opcodeDecode = Just nextMicroOPSource}

-- | Given an instruction and addressing mode generates a list of micro-operations.
-- These micro-operations are encoded as 'transformers' for NOP microOP.
-- Generator omits a final 'CmdDecodeOpcode' microOP necessary to fetch next instruction
-- as it will be appended latger in the pipeline.
microcodeGenerator :: (Instruction, AddressingMode) -> [MicroOP -> MicroOP]
microcodeGenerator (instruction, addressingMode) =
  case (instruction, addressingMode) of
    (NOP, _) -> [decodeAndFetchPC MicroOpcodeBus]
    (PHP, _) -> [pushToStack DATA_WRITE_STATUS . readFromBus DATA_READ, decodeAndFetchPC MicroOpcodeLatch]
    (PLP, _) -> [popFromStack . readFromBus DATA_READ, readFromBus DATA_READ_STATUS, decodeAndFetchPC MicroOpcodeLatch]
    (SEC, _) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
    (CLC, _) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
    (SED, _) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
    (CLD, _) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
    (CLV, _) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
    (SEI, _) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
    (CLI, _) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
    -- Note that JSR saves 'nextPC - 1' onto stack by design! RTS will add the missing 1.
    (JSR, _) ->
      [ -- placeImmediatOnBus, -- Performed during decode.
        -- Currently PCLow is on bus, latch it for later.
        pushToStack DATA_WRITE_PC_HIGH . readFromBus DATA_READ,
        pushToStack DATA_WRITE_PC_LOW,
        placeImmediatOnBus,
        -- At this point PCHigh is on bus, and PCLow is still latched.
        -- Set PC = (PCHigh, PCLow) and fetch next opcode.
        microOPIncrementPC . readFromBus DATA_READ_PC . placeDataOnBus DATA_LATCH_AND_BUS NONE,
        decodeAndFetchPC MicroOpcodeBus
      ]
    (RTS, _) ->
      [ popFromStack,
        popFromStack . readFromBus DATA_READ,
        -- Increment PC one time to get next instruction.
        microOPIncrementPC . readFromBus DATA_READ_PC,
        placeNextOpcodeOnBus,
        decodeAndFetchPC MicroOpcodeBus
      ]
    -- BRK instruction not implemented!
    (BRK, _) -> [decodeAndFetchPC MicroOpcodeBus]
    -- RTI instruction not implemented!
    (RTI, _) -> [decodeAndFetchPC MicroOpcodeBus]
    (JMP, _) -> loadToBus (microOPIncrementPC . readFromBus DATA_READ_PC) Prelude.++ [decodeAndFetchPC MicroOpcodeBus]
    -- TODO: Fetch next opcode on the same cycle the branch is taken.
    (BRANCH _, _) -> [executeCmd, placeNextOpcodeOnBus, decodeAndFetchPC MicroOpcodeBus]
    (Compute _ (ALUConnect _ _ output) _, StackPointer) ->
      case storesToMemory of
        False -> loadStackToBus id Prelude.++ [executeCmd . decodeAndFetchPC MicroOpcodeLatch]
        True -> loadStackToBus (writeAluResult . executeCmd) Prelude.++ [decodeAndFetchPC MicroOpcodeLatch]
      where
        storesToMemory = output == Just Memory
        loadStackToBus :: (MicroOP -> MicroOP) -> [MicroOP -> MicroOP]
        loadStackToBus lastMicroOP =
          [ \microOP ->
              let op = lastMicroOP microOP
               in case _writeData $ _busOp op of
                    Nothing -> popFromStack $ readFromBus DATA_READ op
                    Just writeData -> pushToStack writeData $ readFromBus DATA_READ op
          ]
    (Compute _ (ALUConnect left right output) _, _) ->
      case (usesLoadedData, storesToMemory) of
        (False, False) -> [executeCmd . decodeAndFetchPC MicroOpcodeBus]
        (True, False) -> loadToBus id Prelude.++ [executeCmd . placeNextOpcodeOnBus, decodeAndFetchPC MicroOpcodeBus]
        (False, True) -> loadToBus (writeAluResult . executeCmd) Prelude.++ [placeNextOpcodeOnBus, decodeAndFetchPC MicroOpcodeBus]
        (True, True) -> loadToBus id Prelude.++ [writeToBus LAST_BUS_ADDRESS NONE DATA_WRITE_ALU . executeCmd, placeNextOpcodeOnBus, decodeAndFetchPC MicroOpcodeBus]
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
      Immediate -> []
      Absolute offset ->
        [ -- placeImmediatOnBus,
          placeImmediatOnBus . readFromBus DATA_READ,
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS (addressOffsetToBusAddressOffset offset)
        ]
      ZeroPage offset ->
        [ -- placeImmediatOnBus,
          lastMicroOP . placeDataOnBus BUS_VALUE (addressOffsetToBusAddressOffset offset)
        ]
      Indirect None ->
        [ -- placeImmediatOnBus,
          placeImmediatOnBus . readFromBus DATA_READ,
          -- Load the low-byte of the address, requested address will be latched on bus.
          placeDataOnBus DATA_LATCH_AND_BUS NONE,
          -- Load the high-byte of the address, requested address will be latched on bus.
          placeDataOnBus LAST_BUS_ADDRESS_PLUS_ONE NONE . readFromBus DATA_READ,
          -- Now we have low-byte latched and high-byte on bus, request the actual data.
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS NONE
        ]
      Indirect XRegOffset ->
        [ -- placeImmediatOnBus,
          -- Request address located on zero-page with X offset.
          placeDataOnBus BUS_VALUE REGX,
          -- Load the high-byte of the address, requested address will be latched on bus.
          placeDataOnBus LAST_BUS_ADDRESS_PLUS_ONE NONE . readFromBus DATA_READ,
          -- Now we have low-byte latched and high-byte on bus, request the actual data.
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS NONE
        ]
      Indirect YRegOffset ->
        [ -- placeImmediatOnBus,
          -- Request address located on the zero-page.
          placeDataOnBus BUS_VALUE NONE,
          -- Load the high-byte of the address, requested address will be latched on bus.
          placeDataOnBus LAST_BUS_ADDRESS_PLUS_ONE NONE . readFromBus DATA_READ,
          -- Now we have low-byte latched and high-byte on bus, request the actual data.
          -- Use the value in Y register as offset post-indexing.
          lastMicroOP . placeDataOnBus DATA_LATCH_AND_BUS REGY
        ]
      StackPointer -> errorX "StackPointer addressing handled as a special case"

resetMicroOps :: [MicroOP]
resetMicroOps =
  [ microOPIncrementPC . placeDataOnBus PC NONE $ nopMicroOP,
    microOPIncrementPC . placeDataOnBus PC NONE . readFromBus DATA_READ $ nopMicroOP,
    microOPIncrementPC . readFromBus DATA_READ_PC . placeDataOnBus DATA_LATCH_AND_BUS NONE $ nopMicroOP,
    decodeAndFetchPC MicroOpcodeBus $ nopMicroOP
  ]
