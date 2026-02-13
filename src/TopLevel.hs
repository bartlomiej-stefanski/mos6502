module TopLevel where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Cpu
import Cpu.Data
import Cpu.CpuState
import Cpu.Alu
import Cpu.Microcode.Rom
import Utilities.Utils
import VgaDriver
import MemoryController

topEntity ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  "SWITCHES" ::: Signal System Data ->
  -- ( "VGA_R" ::: Signal System Data,
  --   "VGA_G" ::: Signal System Data,
  --   "VGA_B" ::: Signal System Data,
  --   "VGA_HSYNC" ::: Signal System (Active High),
  --   "VGA_VSYNC" ::: Signal System (Active High),
  --   "VGA_BLANK_N" ::: Signal System (Active Low),
  --   "LEDS" ::: Signal System Data
  -- )
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
    "CARRY_AF" ::: Signal System (Active High),
    "LATCH" ::: Signal System Data
  )
-- topEntity clk rst enable switches = (vgaR, vgaG, vgaB, vgaHSync, vgaVSync, vgaClk, ledData)
topEntity clk rst enable switches = (bitCoerce <$> memAddr, memW, memWData, pc, sp, regA, regX, regY, brkF, intF, negAF, ovfAF, decAF, zeroAF, carryAF, latch)
  where
    -- directBusOp = withClockResetEnable clk rst enable $ cpuMealy (bundle (cpuRamReadData, microOP))
    debugOutputData = withClockResetEnable clk rst enable $ debugCpuMealy (bundle (cpuRamReadData, microOP))
    directBusOp = _directBusOp <$> debugOutputData

    -- directBusOp is combinational circuit output -> it must be latched to guarantee stability
    memAddr = withClockResetEnable clk rst enable $ register 0 (bitCoerce . _addressToQuery <$> directBusOp)
    memW = withClockResetEnable clk rst enable $ register (toActive False) (_shouldWrite <$> directBusOp)
    memWData = withClockResetEnable clk rst enable $ register 0 (_dataToWrite <$> directBusOp)

    -- Ram operation will be latched in ram - it must pass-through here
    cpuRamOp :: Signal System (RamOp AddressSpace Data)
    cpuRamOp = mux (fromActive <$> memW) (RamWrite <$> memAddr <*> memWData) (RamRead <$> memAddr)
    (cpuRamReadData, vgaRamReadData, ledData) = withClockResetEnable clk rst enable $ memoryController cpuRamOp vgaAddr switches

    -- microOpQuery will be latched in microcodeRom - it must pass-through here
    microOPQuery = _microOPQuery <$> directBusOp
    microOP = withClockResetEnable clk rst enable $ microcodeRom microOPQuery

    -- VGA
    vgaDriverRes = withClockResetEnable clk rst enable $ vgaDriver vgaRamReadData
    vgaOutput = snd <$> vgaDriverRes
    vgaAddr = fst <$> vgaDriverRes

    -- vgaR = _vgaR <$> vgaOutput
    -- vgaG = _vgaG <$> vgaOutput
    -- vgaB = _vgaB <$> vgaOutput
    -- vgaHSync = toActive . _hSync <$> vgaOutput
    -- vgaVSync = toActive . _vSync <$> vgaOutput
    -- vgaClk = toActive . _blank <$> vgaOutput

    -- cpuState is 'packed into' mealy output and so it is not latched.
    -- For verilator testing stable values are needed -> latch it here.
    cpuState = withClockResetEnable clk rst enable $ register initCpuState (_debugCpuState <$> debugOutputData)

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

    latch = _dataLatch <$> cpuState

makeTopEntity 'topEntity
