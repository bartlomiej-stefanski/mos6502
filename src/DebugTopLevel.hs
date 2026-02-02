module DebugTopLevel where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Alu
import Cpu.Cpu
import Cpu.CpuState
import Cpu.Data
import Cpu.Microcode.Data
import Cpu.Microcode.Rom
import Utilities.Utils

topEntity ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  "MEM_DATA_IN" ::: Signal System Data ->
  ( "MEM_ADDR" ::: Signal System Addr,
    "MEM_W" ::: Signal System (Active High),
    "MEM_W_DATA" ::: Signal System Data,
    "PC" ::: Signal System Addr,
    "SP" ::: Signal System Data,
    "REG_A" ::: Signal System Data,
    "REG_X" ::: Signal System Data,
    "REG_Y" ::: Signal System Data,
    "BRK_F" ::: Signal System (Active High),
    "INT_F" ::: Signal System (Active High),
    "NEG_AF" ::: Signal System (Active High),
    "OVF_AF" ::: Signal System (Active High),
    "DEC_AF" ::: Signal System (Active High),
    "ZERO_AF" ::: Signal System (Active High),
    "CARRY_AF" ::: Signal System (Active High)
  )
topEntity clk rst enable busInput = (memAddr, memW, memWData, pc, sp, regA, regX, regY, brkF, intF, negAF, ovfAF, decAF, zeroAF, carryAF)
  where
    debugOutputData = withClockResetEnable clk rst enable $ debugCpuMealy (bundle (busInput, currentMicroOP))

    directBusOp = _directBusOp <$> debugOutputData

    currentMicroOP :: Signal System MicroOP
    currentMicroOP = withClockResetEnable clk rst enable $ microcodeRom (_microOPQuery <$> directBusOp)

    memAddr = _addressToQuery <$> directBusOp
    memW = _shouldWrite <$> directBusOp
    memWData = _dataToWrite <$> directBusOp

    cpuState = _debugCpuState <$> debugOutputData
    pc = _regPC <$> cpuState
    sp = _regSP <$> cpuState
    regA = _regA <$> cpuState
    regX = _regX <$> cpuState
    regY = _regY <$> cpuState

    flags = _cpuFlags <$> cpuState
    brkF = _brk <$> flags
    intF = _interrupt <$> flags

    arithFlags = _arithmeticFlags <$> flags
    negAF = _negative <$> arithFlags
    ovfAF = _overflow <$> arithFlags
    decAF = _decimal <$> arithFlags
    zeroAF = _zero <$> arithFlags
    carryAF = _carry <$> arithFlags

makeTopEntity 'topEntity
