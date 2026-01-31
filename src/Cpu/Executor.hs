module Cpu.Executor where

import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.CpuState
import Cpu.Instructions
import Cpu.Microcode
import Utilities.Utils

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
aluInputForCompute aluio cpuState dataOnBus =
  case aluio of
    Just RegA -> _regA cpuState
    Just RegX -> _regX cpuState
    Just RegY -> _regY cpuState
    -- Memory data will be present on bus after load.
    Just Memory -> dataOnBus
    Just RegSP -> _regSP cpuState
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
    arithmeticFlags = case instruction of
      -- PC operations are NEVER in decimal mode
      BRANCH _ -> (_arithmeticFlags flags) {_decimal = toActive False}
      _ -> _arithmeticFlags flags

    instruction = _instruction cpuState

    regX = _regX cpuState
    regPC = _regPC cpuState
    (regPCH, regPCL) = splitAddr regPC

    changeFlags f = cpuState {_cpuFlags = f flags}
    changeArithFlags f = changeFlags $ \fl -> fl {_arithmeticFlags = f arithmeticFlags}

    branchCondition = case instruction of
      BRANCH cond -> cond
      _ -> errorX "Undefined branchCondition value for non (BRANCH) operation"

    takeBranch = shouldTakeBranch branchCondition arithmeticFlags
    branchAdditionOverflow = fromActive (_carry aluArithmeticFlags)
    branchOffsetNegative = testBit dataOnBus 7
    aluCarryAddon = case (branchAdditionOverflow, branchOffsetNegative) of
      (True, False) -> 1
      (False, True) -> 0xff :: Data
      _ -> 0
    pcAfterBranch =
      if takeBranch
        then bitCoerce (regPCH + aluCarryAddon, aluResult)
        else regPC

    aluOp = case instruction of
      Compute op _ -> op
      BRANCH _ -> ALU_ADD False
      _ -> errorX "Undefined aluOP for non (BRANCH | COMPUTE) operation"

    (aluInputX, aluInputY) = case instruction of
      Compute _ (ALUConnect left right _) ->
        (aluInputForCompute left cpuState dataOnBus, aluInputForCompute (Just right) cpuState dataOnBus)
      BRANCH _ -> (regPCL, dataOnBus)
      _ -> errorX "Undefined aluInput for non (BRANCH | COMPUTE) operation"

    (aluResult, aluArithmeticFlags) = alu aluOp arithmeticFlags aluInputX aluInputY

data InputData
  = InputData
  { _busData :: Maybe Data,
    _microOP :: MicroOP
  }
  deriving (Eq, Show, Generic, NFDataX)

data OutputData
  = OutputData
  { _busAddress :: Addr,
    _busWriteData :: Maybe Data,
    -- | Used by the CPU to indicate the next microcode operation to execute.
    _nextMicroOp :: Maybe MicroOpIndex
  }
  deriving (Eq, Show, Generic, NFDataX)

-- | Executes a micro-operation on the CPU state and input data.
-- This implements a function of type (s -> i -> (s, o)) to be used
-- with the mealy combinator.
cpuExecutor :: CpuState -> InputData -> (CpuState, OutputData)
cpuExecutor cpuState inputData = (outCpuState, outputData)
  where
    microOP = _microOP inputData
    cmd = _cmd microOP

    dataOnBus = _busData inputData
    (regPCH, regPCL) = splitAddr $ _regPC cpuState

    busOP = _busOp microOP
    readData = _readData busOP
    applyReadData cpuS = case readData of
      Just DATA_READ_PC_LOW -> cpuS {_regPC = bitCoerce (regPCH, fromJustX dataOnBus)}
      Just DATA_READ_PC_HIGH -> cpuS {_regPC = bitCoerce (fromJustX dataOnBus, regPCL)}
      Just DATA_READ_STATUS -> cpuS {_cpuFlags = cpuFlagsFromData $ fromJustX dataOnBus}
      Just DATA_READ -> cpuS {_dataLatch = fromJustX dataOnBus}
      Nothing -> cpuS

    applyPcChange cpuS =
      if _incrementPC microOP
        then cpuS {_regPC = _regPC cpuS + 1}
        else cpuS

    rawBusAddress :: Addr
    rawBusAddress = case _address busOP of
      SP -> zeroExtend $ _regSP cpuState
      PC -> _regPC cpuState
      BUS_VALUE -> zeroExtend $ fromJustX dataOnBus
      DATA_LATCH_AND_BUS -> bitCoerce (_dataLatch cpuState, fromJustX dataOnBus)
    busAddress = case _addressOffset busOP of
      NONE -> rawBusAddress
      REGX -> rawBusAddress + zeroExtend (_regX cpuState)
      REGY -> rawBusAddress + zeroExtend (_regY cpuState)

    busWriteData = case _writeData busOP of
      Just DATA_WRITE_PC_LOW -> Just regPCL
      Just DATA_WRITE_PC_HIGH -> Just regPCH
      Just DATA_WRITE_STATUS -> Just $ cpuFlagsToData $ _cpuFlags cpuState
      Just DATA_WRITE_ALU -> Just aluOutputData
      Nothing -> Nothing

    (postExecCpuState, aluOutputData) =
      executeCpuInstruction (fromJustX dataOnBus) cpuState

    baseOutputData =
      OutputData
        { _busAddress = busAddress,
          _busWriteData = busWriteData,
          _nextMicroOp = Nothing
        }

    outputData = case cmd of
      CmdExecute -> baseOutputData
      CmdDecodeOpcode -> setNextMicroOp baseOutputData
      CmdNOP -> baseOutputData

    (nextInstruction, nextAddressingMode) = decode (fromJustX dataOnBus)
    nextMicroOpIndex = getNextMicrocodeIndex (nextInstruction, nextAddressingMode)
    setNextMicroOp oData = oData {_nextMicroOp = Just nextMicroOpIndex}
    setNextInstruction cpuS = cpuS {_instruction = nextInstruction}

    outCpuState = applyPcChange . applyReadData $ case cmd of
      CmdExecute -> postExecCpuState
      CmdDecodeOpcode -> setNextInstruction cpuState
      CmdNOP -> cpuState

cpuMealy ::
  (HiddenClockResetEnable dom) =>
  CpuState ->
  Signal dom InputData ->
  Signal dom OutputData
cpuMealy = mealy cpuExecutor
