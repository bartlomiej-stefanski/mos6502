module Cpu.Cpu where

import Clash.Prelude
import Cpu.Alu
import Cpu.CpuState
import Cpu.Data
import Cpu.Executor
import Cpu.Microcode.Data
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
      _microcodeLatch = 0
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
  ( CpuStateWithBus
      { _cpuState = cpuS,
        _busAddressLatch = case _busAddress outData of
          Just addr -> addr
          Nothing -> _busAddressLatch cpuStateWithPBus,
        _microcodeLatch = nextMicrocode
      },
    DirectBusOp
      { _addressToQuery = fromJustX $ _busAddress outData,
        _dataToWrite = fromJustX $ _busWriteData outData,
        _shouldWrite = shouldWrite,
        _microOPQuery = fromJustX $ _nextMicroOp outData
      }
  )
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
