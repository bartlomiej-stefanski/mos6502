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

shouldTakeBranch :: BranchCondition -> ArithmeticFlags -> Bool
shouldTakeBranch = \case
  OnPlus -> not . fromActive . _negative
  OnMinus -> fromActive . _negative
  OnOverflowClear -> not . fromActive . _overflow
  OnOverflowSet -> fromActive . _overflow
  OnCarryClear -> not . fromActive . _carry
  OnCarrySet -> fromActive . _carry
  OnNotEqual -> not . fromActive . _zero
  OnEqual -> fromActive . _zero

aluInputForCompute :: Maybe ALUIO -> CpuState -> Data -> Data
aluInputForCompute aluio cpuState busInput =
  case aluio of
    Just RegA -> _regA cpuState
    Just RegX -> _regX cpuState
    Just RegY -> _regY cpuState
    Just Memory -> busInput
    Just RegSP -> busInput
    Just One -> 1
    Nothing -> errorX "Empty ALUIO input"

executeCpuInstruction :: Data -> CpuState -> (CpuState, Data)
executeCpuInstruction dataOnBus cpuState =
  let newCpuState = case instruction of
        TXS -> cpuState {_regSP = regX}
        SEC -> changeArithFlags \f -> f {_carry = toActive True}
        CLC -> changeArithFlags \f -> f {_carry = toActive False}
        SED -> changeArithFlags \f -> f {_decimal = toActive True}
        CLD -> changeArithFlags \f -> f {_decimal = toActive False}
        CLV -> changeArithFlags \f -> f {_overflow = toActive False}
        SEI -> changeFlags \f -> f {_interrupt = toActive True}
        CLI -> changeFlags \f -> f {_interrupt = toActive False}
        JMP -> cpuState {_regPC = bitCoerce (dataOnBus, dataLatch)}
        BRANCH _ -> cpuState {_regPC = pcAfterBranch}
        Compute _ (ALUConnect _ _ aluOut) ->
          let registerChange s = case aluOut of
                Just RegA -> s {_regA = aluResult}
                Just RegX -> s {_regX = aluResult}
                Just RegY -> s {_regY = aluResult}
                _ -> s
          in registerChange $ changeFlags \f -> f {_arithmeticFlags = aluArithmeticFlags}
        _ -> errorX "Not Supported Instruction in executeCpuInstruction"
   in (newCpuState, aluResult)
  where
    flags = _cpuFlags cpuState
    arithmeticFlags = _arithmeticFlags flags

    instruction = _instruction cpuState

    regX = _regX cpuState
    regPC = _regPC cpuState
    regPCH = bitCoerce $ slice d15 d8 regPC :: Data
    regPCL = bitCoerce $ slice d7 d0 regPC :: Data
    dataLatch = _dataLatch cpuState

    changeFlags f = cpuState {_cpuFlags = f flags}
    changeArithFlags f = changeFlags $ \fl -> fl {_arithmeticFlags = f arithmeticFlags}

    branchCondition = case instruction of
      BRANCH cond -> cond
      _ -> errorX "Undefined branchCondition value for non (BRANCH) operation"

    takeBranch = shouldTakeBranch branchCondition arithmeticFlags
    aluCarryAddon = if fromActive (_carry aluArithmeticFlags) then 1 else 0
    pcAfterBranch = if takeBranch
      then bitCoerce (regPCH + aluCarryAddon, aluResult)
      else regPC

    aluOp = case instruction of
      Compute op _ -> op
      BRANCH _ -> ALU_ADD True
      _ -> errorX "Undefined aluOP for non (BRANCH | COMPUTE) operation"

    (aluInputX, aluInputY) = case instruction of
      Compute _ (ALUConnect left right _) ->
        (aluInputForCompute left cpuState dataOnBus, aluInputForCompute (Just right) cpuState dataOnBus)
      BRANCH _ -> (regPCL, dataLatch)
      _ -> errorX "Undefined aluInput for non (BRANCH | COMPUTE) operation"

    (aluResult, aluArithmeticFlags) = alu aluOp arithmeticFlags aluInputX aluInputY

-- | Executes a micro-operation on the CPU state and input data.
-- This implemensts a functionn of type (s -> i -> (s, o)) to be used
-- with the mealy combinator.
-- cpuExecutor :: CpuState -> InputData -> (CpuState, OutputData)
-- cpuExecutor cpuState inputData =
--   let nextAddress = case microOpBusAddress of
--         PC -> _regPC initialState
--         SP -> stackOffset + zeroExtend (_regSP initialState)
--         COMPUTE_FROM_MODE -> getAddressFromMode initialState

--   let outputData =
--         OutputData
--           { busAddress = nextAddress,
--             writeOutputData = Nothing,
--             nextMicroOp = errorX "Not implemented"
--           }

--   modify $ \s -> case opReadData of
--     Just DATA_READ_PC_LOW -> s {_dataLatch = dataOnBus}
--     Just DATA_READ_PC_HIGH ->
--       s {_regPC = bitCoerce (dataOnBus, _dataLatch s)}
--     _ -> s

--   case microCmd of
--     CmdDecodeOpcode -> do
--       modify $ \s ->
--         s
--           { _instruction = nextInstruction,
--             _addressingMode = nextAddressingMode,
--             _regPC = _regPC s + 1
--           }
--       return outputData
--     CmdNOP -> do
--       return outputData
--     CmdExecute -> do
--       modify $ executeCpuInstruction dataOnBus
--       return outputData
--   where
--     microCmd = _cmd op
--     opOnBus = _busOp op
--     microOpBusAddress = _address opOnBus
--     opReadData = _readData opOnBus

--     (nextInstruction, nextAddressingMode) = decode dataOnBus

-- cpuMealy ::
--   (HiddenClockResetEnable dom) =>
--   CpuState ->
--   Signal dom InputData ->
--   Signal dom OutputData
-- cpuMealy = mealy cpuExecutor
