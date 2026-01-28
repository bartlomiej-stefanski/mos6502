module Cpu.Executor where

import Clash.Prelude
import Control.Monad.State.Lazy
import Cpu.Alu
import Cpu.Cpu
import Cpu.CpuState
import Cpu.Instructions
import Cpu.Microcode
import Utilities.Utils

data InputData
  = InputData
  { busData :: Data,
    microOP :: MicroOP
  }
  deriving (Eq, Show, Generic, NFDataX)

data OutputData
  = OutputData
  { busAddress :: Addr,
    writeOutputData :: Maybe Data,
    nextMicroOp :: Index 1024
  }
  deriving (Eq, Show, Generic, NFDataX)

type CpuM a = State CpuState a

getAddressFromMode :: CpuState -> Addr
getAddressFromMode = errorX "Not implemented!"

executeCpuInstruction :: Data -> CpuM OutputData
executeCpuInstruction dataOnBus = do

  cpuState <- get
  -- let flags = cpuFlags cpuState

  let no_data = return $ errorX "No Data"

  case instruction cpuState of
    TXS -> do
      modify $ \s -> s { regSP = regX s }
      no_data
    SEC -> do
      modify $ \s -> changeFlags (changeArithFlag (\f -> f { carry = toActive True})) s
      no_data
    CLC -> do
      modify $ \s -> changeFlags (changeArithFlag (\f -> f { carry = toActive False})) s
      no_data
    SED -> do
      modify $ \s -> changeFlags (changeArithFlag (\f -> f { decimal = toActive True})) s
      no_data
    CLD -> do
      modify $ \s -> changeFlags (changeArithFlag (\f -> f { decimal = toActive False})) s
      no_data
    CLV -> do
      modify $ \s -> changeFlags (changeArithFlag (\f -> f { overflow = toActive False})) s
      no_data
    SEI -> do
      modify $ \s -> changeFlags (\f -> f { interrupt = toActive True}) s
      no_data
    CLI -> do
      modify $ \s -> changeFlags (\f -> f { interrupt = toActive False}) s
      no_data
    JMP -> do
      modify $ \s -> s { regPC = bitCoerce (dataOnBus, dataLatch s) }
      no_data
    BRANCH branchCondition ->
      errorX "Not Implemented"
    Compute aluOp aluConnection ->
      errorX "Not Implemented"
    _ -> no_data

  where
    changeFlags op _cpuState = _cpuState { cpuFlags = op (cpuFlags _cpuState) }

    changeArithFlag op flags =
      flags { arithmeticFlags = newArithFlags  }
      where
        currArithFlags = arithmeticFlags flags
        newArithFlags = op currArithFlags


-- | Actuall cpu logic.
cpuLogic :: InputData -> CpuM OutputData
cpuLogic (InputData dataOnBus op) = do
  initialState <- get
  let nextAddress = case microOpBusAddress of
        PC -> regPC initialState
        SP -> stackOffset + zeroExtend (regSP initialState)
        COMPUTE_FROM_MODE -> getAddressFromMode initialState

  let outputData =
        OutputData
          { busAddress = nextAddress,
            writeOutputData = Nothing,
            nextMicroOp = errorX "Not implemented"
          }

  modify $ \s -> case opReadData of
    Just DATA_READ_PC_LOW -> s {dataLatch = dataOnBus}
    Just DATA_READ_PC_HIGH ->
      s {regPC = bitCoerce (dataOnBus, dataLatch s)}
    _ -> s

  case microCmd of
    CmdDecodeOpcode -> do
      modify $ \s ->
        s
          { instruction = nextInstruction,
            addressingMode = nextAddressingMode,
            regPC = regPC s + 1
          }
      return outputData
    CmdNOP -> do
      return outputData
    CmdExecute -> do
      aluData <- executeCpuInstruction dataOnBus
      return outputData
  where
    microCmd = cmd op
    opOnBus = busOp op
    microOpBusAddress = address opOnBus
    opReadData = readData opOnBus

    (nextInstruction, nextAddressingMode) = decode dataOnBus

-- | Executes a micro-operation on the CPU state and input data.
-- This implemensts a functionn of type (s -> i -> (s, o)) to be used
-- with the mealy combinator.
cpuExecutor :: CpuState -> InputData -> (CpuState, OutputData)
cpuExecutor cpuState inputData =
  let (outputData, newCpuState) = runState (cpuLogic inputData) cpuState
   in (newCpuState, outputData)

cpuMealy ::
  (HiddenClockResetEnable dom) =>
  CpuState ->
  Signal dom InputData ->
  Signal dom OutputData
cpuMealy = mealy cpuExecutor
