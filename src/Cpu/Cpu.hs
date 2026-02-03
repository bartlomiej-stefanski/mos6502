module Cpu.Cpu where

import Clash.Prelude
import Cpu.CpuState
import Cpu.Data
import Cpu.Executor
import Cpu.Instructions
import Cpu.Microcode.Data
import Cpu.Microcode.Rom
import Utilities.Utils

data CpuStateWithBus
  = CpuStateWithBus
  { _cpuState :: CpuState,
    _busAddressLatch :: Addr,
    _microcodeLatch :: MicroOPRomAddress
  }
  deriving (Eq, Show, Generic, NFDataX)

initCpuStateWithBus :: CpuStateWithBus
initCpuStateWithBus =
  CpuStateWithBus
    { _cpuState = initCpuState,
      _busAddressLatch = 0,
      _microcodeLatch = opcodeMapperRom !! jmpAbsoluteOpcode
    }

data DirectBusOp
  = DirectBusOp
  { _addressToQuery :: Addr,
    _dataToWrite :: Data,
    _shouldWrite :: Active High,
    _microOPQuery :: MicroOPRomAddress
  }
  deriving (Eq, Show, Generic, NFDataX)

cpuWithBus :: CpuStateWithBus -> (Data, MicroOP) -> (CpuStateWithBus, DirectBusOp)
cpuWithBus cpuStateWithPBus (busData, microOP) =
  if shouldSyncRom then syncResult else normalOperationResult
  where
    inputData =
      InputData
        { _busData = busData,
          _lastBusAddress = _busAddressLatch cpuStateWithPBus,
          _microOP = microOP
        }

    (cpuS, outData) = cpuExecutor (_cpuState cpuStateWithPBus) inputData

    shouldWrite = case _busWriteData outData of
      Just _ -> toActive True
      Nothing -> toActive False

    nextMicrocode = case _nextMicroOp outData of
      Just idx -> idx
      Nothing -> _microcodeLatch cpuStateWithPBus + 1

    queryAddress = case _busAddress outData of
      Just addr -> addr
      Nothing -> _busAddressLatch cpuStateWithPBus

    normalOperationResult =
      ( CpuStateWithBus
          { _cpuState = cpuS,
            _busAddressLatch = queryAddress,
            _microcodeLatch = nextMicrocode
          },
        DirectBusOp
          { _addressToQuery = queryAddress,
            -- If we do not write the data is irrelevant.
            _dataToWrite = fromJustX $ _busWriteData outData,
            _shouldWrite = shouldWrite,
            _microOPQuery = nextMicrocode
          }
      )

    shouldSyncRom = _sync_after_reset (_cpuState cpuStateWithPBus)
    syncResult =
      ( initCpuStateWithBus {_cpuState = (_cpuState initCpuStateWithBus) {_sync_after_reset = False}},
        DirectBusOp
          { _addressToQuery = 0,
            _dataToWrite = 0,
            _shouldWrite = toActive False,
            _microOPQuery = _microcodeLatch cpuStateWithPBus
          }
      )

cpuMealy ::
  (HiddenClockResetEnable dom) =>
  Signal dom (Data, MicroOP) ->
  Signal dom DirectBusOp
cpuMealy = mealy cpuWithBus initCpuStateWithBus

data DebugOutputData
  = DebugOutputData
  { _directBusOp :: DirectBusOp,
    _debugCpuState :: CpuState
  }
  deriving (Eq, Show, Generic, NFDataX)

-- | Mealy CPU with additional debug data in OutputData.
debugCpuMealy ::
  (HiddenClockResetEnable dom) =>
  Signal dom (Data, MicroOP) ->
  Signal dom DebugOutputData
debugCpuMealy = mealy debugCpuExecutor initCpuStateWithBus
  where
    debugCpuExecutor :: CpuStateWithBus -> (Data, MicroOP) -> (CpuStateWithBus, DebugOutputData)
    debugCpuExecutor initState inputData =
      let (newCpuState, busOP) = cpuWithBus initState inputData
       in (newCpuState, DebugOutputData busOP (_cpuState newCpuState))
