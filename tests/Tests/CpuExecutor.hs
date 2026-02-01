module Tests.CpuExecutor where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.CpuState
import Cpu.Executor
import Cpu.Instructions
import Cpu.Microcode
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Tests.CpuGenerator
import Utilities.Utils
import qualified Prelude

nopBusOP :: BusOP
nopBusOP =
  BusOP
    { _address = PC,
      _addressOffset = NONE,
      _writeData = Nothing,
      _readData = Nothing
    }

nopMicroOP :: MicroOP
nopMicroOP =
  MicroOP
    { _cmd = CmdNOP,
      _busOp = nopBusOP,
      _incrementPC = False
    }

nopInputData :: InputData
nopInputData =
  InputData
    { _busData = Nothing,
      _microOP = nopMicroOP
    }

pcIncrementer :: Bool -> CpuState -> CpuState
pcIncrementer inc cpuState =
  if inc
    then cpuState {_regPC = _regPC cpuState + 1}
    else cpuState

prop_nop_does_nothing :: H.Property
prop_nop_does_nothing = H.property do
  cpuState <- H.forAll genCpuState

  let (newState, outputData) = cpuExecutor cpuState nopInputData
  _busWriteData outputData H.=== Nothing
  _nextMicroOp outputData H.=== Nothing
  _busAddress outputData H.=== _regPC cpuState
  newState H.=== cpuState

prop_handles_bus_read :: H.Property
prop_handles_bus_read = H.property do
  cpuState <- H.forAll genCpuState
  busInput <- H.forAll genData

  readOp <- H.forAll $ Gen.choice $ Prelude.map return [DATA_READ, DATA_READ_PC_LOW, DATA_READ_PC_HIGH, DATA_READ_STATUS]
  incrementPC <- H.forAll Gen.bool

  let busOp = nopBusOP {_readData = Just readOp}
  let inputData =
        nopInputData
          { _microOP = nopMicroOP {_busOp = busOp, _incrementPC = incrementPC},
            _busData = Just busInput
          }
  let expectedState = pcIncrementer incrementPC $ case readOp of
        DATA_READ ->
          cpuState {_dataLatch = busInput}
        DATA_READ_PC_LOW ->
          let (pcHigh, _) = splitAddr $ _regPC cpuState
           in cpuState {_regPC = bitCoerce (pcHigh, busInput)}
        DATA_READ_PC_HIGH -> do
          let (_, pcLow) = splitAddr $ _regPC cpuState
           in cpuState {_regPC = bitCoerce (busInput, pcLow)}
        DATA_READ_STATUS ->
          let expectedFlags = cpuFlagsFromData busInput
           in cpuState {_cpuFlags = expectedFlags}

  let (newState, outputData) = cpuExecutor cpuState inputData
  newState H.=== expectedState
  outputData
    H.=== OutputData
      { _busAddress = _regPC cpuState,
        _busWriteData = Nothing,
        _nextMicroOp = Nothing
      }

prop_handles_bus_write :: H.Property
prop_handles_bus_write = H.property do
  cpuState <- H.forAll genCpuState
  busInput <- H.forAll genData
  cpuDataLatch <- H.forAll genData

  writeOp <- H.forAll $ Gen.choice $ Prelude.map return [DATA_WRITE_PC_LOW, DATA_WRITE_PC_HIGH, DATA_WRITE_STATUS]
  address <- H.forAll $ Gen.choice $ Prelude.map return [SP, PC, BUS_VALUE, DATA_LATCH_AND_BUS]
  addressOffset <- H.forAll $ Gen.choice $ Prelude.map return [NONE, REGX, REGY]
  incrementPC <- H.forAll Gen.bool

  let busOp =
        nopBusOP
          { _writeData = Just writeOp,
            _address = address,
            _addressOffset = addressOffset
          }
  let inputData =
        nopInputData
          { _microOP = nopMicroOP {_busOp = busOp, _incrementPC = incrementPC},
            _busData = Just busInput
          }

  let cpuWithLatch = cpuState {_dataLatch = cpuDataLatch}

  let (x, y, pc, sp) = (_regX cpuState, _regY cpuState, _regPC cpuState, _regSP cpuState)
  let offset :: Addr = case addressOffset of
        NONE -> 0
        REGX -> zeroExtend x
        REGY -> zeroExtend y
  let expectedBusAddress =
        offset + case address of
          SP -> zeroExtend sp
          PC -> pc
          BUS_VALUE -> zeroExtend busInput
          DATA_LATCH_AND_BUS -> bitCoerce (busInput, cpuDataLatch)

  let busWriteData = case writeOp of
        DATA_WRITE_PC_LOW -> snd $ splitAddr pc
        DATA_WRITE_PC_HIGH -> fst $ splitAddr pc
        DATA_WRITE_STATUS -> cpuFlagsToData $ _cpuFlags cpuState
        DATA_WRITE_ALU -> errorX "Impossible - not checked here"

  let (newState, outputData) = cpuExecutor cpuWithLatch inputData
  newState H.=== pcIncrementer incrementPC cpuWithLatch
  outputData
    H.=== OutputData
      { _busAddress = expectedBusAddress,
        _busWriteData = Just busWriteData,
        _nextMicroOp = Nothing
      }

prop_handles_bus_read_and_write :: H.Property
prop_handles_bus_read_and_write = H.property do
  cpuState <- H.forAll genCpuState
  busInput <- H.forAll genData
  cpuDataLatch <- H.forAll genData

  readOp <- H.forAll $ Gen.choice $ Prelude.map return [DATA_READ, DATA_READ_PC_LOW, DATA_READ_PC_HIGH, DATA_READ_STATUS]
  writeOp <- H.forAll $ Gen.choice $ Prelude.map return [DATA_WRITE_PC_LOW, DATA_WRITE_PC_HIGH, DATA_WRITE_STATUS]
  address <- H.forAll $ Gen.choice $ Prelude.map return [SP, PC, BUS_VALUE, DATA_LATCH_AND_BUS]
  addressOffset <- H.forAll $ Gen.choice $ Prelude.map return [NONE, REGX, REGY]
  incrementPC <- H.forAll Gen.bool

  let busOp =
        nopBusOP
          { _writeData = Just writeOp,
            _address = address,
            _addressOffset = addressOffset,
            _readData = Just readOp
          }
  let inputData =
        nopInputData
          { _microOP = nopMicroOP {_busOp = busOp, _incrementPC = incrementPC},
            _busData = Just busInput
          }

  let cpuWithLatch = cpuState {_dataLatch = cpuDataLatch}
  let expectedState = pcIncrementer incrementPC $ case readOp of
        DATA_READ ->
          cpuWithLatch {_dataLatch = busInput}
        DATA_READ_PC_LOW ->
          let (pcHigh, _) = splitAddr $ _regPC cpuState
           in cpuWithLatch {_regPC = bitCoerce (pcHigh, busInput)}
        DATA_READ_PC_HIGH -> do
          let (_, pcLow) = splitAddr $ _regPC cpuState
           in cpuWithLatch {_regPC = bitCoerce (busInput, pcLow)}
        DATA_READ_STATUS ->
          let expectedFlags = cpuFlagsFromData busInput
           in cpuWithLatch {_cpuFlags = expectedFlags}

  let (x, y, pc, sp) = (_regX cpuState, _regY cpuState, _regPC cpuState, _regSP cpuState)
  let offset :: Addr = case addressOffset of
        NONE -> 0
        REGX -> zeroExtend x
        REGY -> zeroExtend y
  let expectedBusAddress =
        offset + case address of
          SP -> zeroExtend sp
          PC -> pc
          BUS_VALUE -> zeroExtend busInput
          DATA_LATCH_AND_BUS -> bitCoerce (busInput, cpuDataLatch)

  let busWriteData = case writeOp of
        DATA_WRITE_PC_LOW -> snd $ splitAddr pc
        DATA_WRITE_PC_HIGH -> fst $ splitAddr pc
        DATA_WRITE_STATUS -> cpuFlagsToData $ _cpuFlags cpuState
        DATA_WRITE_ALU -> errorX "Impossible - not checked here"

  let (newState, outputData) = cpuExecutor cpuWithLatch inputData
  newState H.=== expectedState
  outputData
    H.=== OutputData
      { _busAddress = expectedBusAddress,
        _busWriteData = Just busWriteData,
        _nextMicroOp = Nothing
      }

prop_handles_flag_operations :: H.Property
prop_handles_flag_operations = H.property do
  cpuState <- H.forAll genCpuState

  instruction <- H.forAll $ Gen.choice $ Prelude.map return [SEC, CLC, SED, CLD, CLV, SEI, CLI]

  let inputData = nopInputData {_microOP = nopMicroOP {_cmd = CmdExecute}}

  let cpuStateWithInstruction = cpuState {_instruction = instruction}

  let flags = _cpuFlags cpuStateWithInstruction
  let arithmeticFlags = _arithmeticFlags flags
  let expected_flags = case instruction of
        SEC -> flags {_arithmeticFlags = arithmeticFlags {_carry = toActive True}}
        CLC -> flags {_arithmeticFlags = arithmeticFlags {_carry = toActive False}}
        SED -> flags {_arithmeticFlags = arithmeticFlags {_decimal = toActive True}}
        CLD -> flags {_arithmeticFlags = arithmeticFlags {_decimal = toActive False}}
        SEI -> flags {_interrupt = toActive True}
        CLI -> flags {_interrupt = toActive False}
        CLV -> flags {_arithmeticFlags = arithmeticFlags {_overflow = toActive False}}
        _ -> errorX ("Impossible - not checked here" Prelude.++ show instruction)
  let expectedCpuState = cpuStateWithInstruction {_cpuFlags = expected_flags}

  let (newState, outputData) = cpuExecutor cpuStateWithInstruction inputData
  newState H.=== expectedCpuState
  outputData
    H.=== OutputData
      { _busAddress = _regPC cpuState,
        _busWriteData = Nothing,
        _nextMicroOp = Nothing
      }

prop_handles_branch_operations :: H.Property
prop_handles_branch_operations = H.property do
  cpuState <- H.forAll genCpuState
  busInput <- H.forAll genData

  incrementPC <- H.forAll Gen.bool
  branchCondition <-
    H.forAll
      $ Gen.choice
      $ Prelude.map
        return
        [OnPlus, OnMinus, OnCarrySet, OnCarryClear, OnOverflowSet, OnOverflowClear]

  let inputData =
        nopInputData
          { _microOP = nopMicroOP {_cmd = CmdExecute, _incrementPC = incrementPC},
            _busData = Just busInput
          }
  let cpuWithInstruction = cpuState {_instruction = BRANCH branchCondition}
  let shouldJump = shouldTakeBranch branchCondition (_arithmeticFlags $ _cpuFlags cpuWithInstruction)
  -- Use integer to test negative offsets
  let offset = toInteger busInput :: Integer
  let regPC = toInteger $ _regPC cpuWithInstruction :: Integer
  let expectedPC :: Addr =
        fromInteger
          $ if shouldJump
            then regPC + if offset > 0x7f then offset - 0x100 else offset
            else regPC

  let expectedState = pcIncrementer incrementPC $ cpuWithInstruction {_regPC = expectedPC}

  let (newState, outputData) = cpuExecutor cpuWithInstruction inputData
  newState H.=== expectedState
  outputData
    H.=== OutputData
      { _busAddress = _regPC cpuState,
        _busWriteData = Nothing,
        _nextMicroOp = Nothing
      }

prop_handles_alu_operations :: H.Property
prop_handles_alu_operations = H.property do
  cpuState <- H.forAll genCpuState
  busInput <- H.forAll genData

  aluOp <- H.forAll genAluOp
  incrementPC <- H.forAll Gen.bool

  leftAluConnect <- H.forAll $ Gen.choice $ Prelude.map return [RegA, RegX, RegY, RegSP, One]
  rightAluConnect <- H.forAll $ Gen.choice $ Prelude.map return [RegA, RegX, RegY, RegSP, One]
  outAluConnect <- H.forAll $ Gen.choice $ Prelude.map return [Just RegA, Just RegX, Just RegY, Nothing]
  let aluConnect = ALUConnect (Just leftAluConnect) rightAluConnect outAluConnect

  writeData <- H.forAll $ Gen.choice $ Prelude.map return [Just DATA_WRITE_ALU, Nothing]

  updateFlags <- H.forAll Gen.bool

  let instruction = Compute aluOp aluConnect updateFlags

  let busOP =
        nopBusOP
          { _writeData = writeData,
            _address = PC,
            _addressOffset = NONE
          }
  let microOP = nopMicroOP {_cmd = CmdExecute, _busOp = busOP, _incrementPC = incrementPC}
  let inputData =
        nopInputData
          { _microOP = microOP,
            _busData = Just busInput
          }
  let cpuWithInstruction = cpuState {_instruction = instruction}

  let x = _regX cpuWithInstruction
  let y = _regY cpuWithInstruction
  let a = _regA cpuWithInstruction
  let sp = _regSP cpuWithInstruction

  let valueFromConnect = \case
        RegA -> a
        RegX -> x
        RegY -> y
        RegSP -> sp
        One -> 1
        _ -> errorX "Impossible - not checked here"

  let arithmeticFlags = _arithmeticFlags $ _cpuFlags cpuWithInstruction
  let leftInput = valueFromConnect leftAluConnect
  let rightInput = valueFromConnect rightAluConnect
  let (expectedValue, expectedArithFlags) = alu aluOp arithmeticFlags leftInput rightInput

  let flags = _cpuFlags cpuWithInstruction
  let expectedCpuState =
        pcIncrementer incrementPC
          $ ( case outAluConnect of
                Just RegA -> cpuWithInstruction {_regA = expectedValue}
                Just RegX -> cpuWithInstruction {_regX = expectedValue}
                Just RegY -> cpuWithInstruction {_regY = expectedValue}
                _ -> cpuWithInstruction
            )
            { _cpuFlags =
                flags
                  { _arithmeticFlags =
                      if updateFlags
                        then expectedArithFlags
                        else arithmeticFlags
                  }
            }

  let expectedOutputData =
        OutputData
          { _busAddress = _regPC cpuWithInstruction,
            _busWriteData =
              case writeData of
                Just DATA_WRITE_ALU -> Just expectedValue
                _ -> Nothing,
            _nextMicroOp = Nothing
          }

  let (newState, outputData) = cpuExecutor cpuWithInstruction inputData
  newState H.=== expectedCpuState
  outputData H.=== expectedOutputData

cpuExecutorTests :: TestTree
cpuExecutorTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain cpuExecutorTests
