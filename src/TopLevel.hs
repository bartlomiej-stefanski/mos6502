module TopLevel where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Cpu
import Cpu.Data
import Cpu.Microcode.Rom
import VgaDriver
import Utilities.Utils

type CpuMemoryOp = RamOp AddressSpace Data

codeRomFile :: FilePath
codeRomFile = "roms/code.rom"
mos6502CodeRom :: (HiddenClockResetEnable dom) =>
  Signal dom (Unsigned 15) ->
  Signal dom (BitVector 8)
mos6502CodeRom = romFile (pow2SNat d15) codeRomFile

getCodeRomQuery :: CpuMemoryOp -> Unsigned 15
getCodeRomQuery (RamRead addr) = bitCoerce $ slice d14 d0 addr
getCodeRomQuery _ = errorX "Instruction ROM does not support writes."


mos6502VgaMemory :: (HiddenClockResetEnable dom) =>
  Signal dom (RamOp VgaRamSize Data) ->
  Signal dom (RamOp VgaRamSize Data) ->
  (Signal dom Data, Signal dom Data)
mos6502VgaMemory = trueDualPortBlockRam

getVgaQuery :: CpuMemoryOp -> RamOp VgaRamSize Data
getVgaQuery (RamRead addr) = if addr >= (bitCoerce vgaBufferStart) && addr < (bitCoerce vgaBufferEnd)
  then RamRead (bitCoerce (slice d10 d0 addr))
  else RamNoOp
getVgaQuery _ = RamNoOp


memoryController :: (HiddenClockResetEnable dom) =>
  Signal dom CpuMemoryOp ->
  Signal dom VgaMemoryOp ->
  (Signal dom Data, Signal dom Data)
memoryController cpuRamOp vgaOp = (cpuData, vgaData)
  where
    cpuData = pure 0

    cpuVgaOp = getVgaQuery <$> cpuRamOp
    (cpuVgaData, vgaData) = mos6502VgaMemory cpuVgaOp vgaOp

    cpuCodeData = mos6502CodeRom $ getCodeRomQuery <$> cpuRamOp


topEntity ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  ( "VGA_R" ::: Signal System Data,
    "VGA_G" ::: Signal System Data,
    "VGA_B" ::: Signal System Data,
    "VGA_HSYNC" ::: Signal System (Active High),
    "VGA_VSYNC" ::: Signal System (Active High),
    "VGA_BLANK_N" ::: Signal System (Active Low)
  )
topEntity clk rst enable = (vgaR, vgaG, vgaB, vgaHSync, vgaVSync, vgaClk)
  where
    directBusOp = withClockResetEnable clk rst enable $ cpuMealy (bundle (cpuRamReadData, microOP))

    -- directBusOp is combinational circuit output -> it must be latched to guarantee stability
    memAddr = bitCoerce <$> _addressToQuery <$> directBusOp :: Signal System (Index AddressSpace)
    memW = _shouldWrite <$> directBusOp
    memWData = _dataToWrite <$> directBusOp

    -- Ram operation will be latched in ram - it must pass-through here
    cpuRamOp :: Signal System (RamOp AddressSpace Data)
    cpuRamOp = mux (fromActive <$> memW) (RamWrite <$> memAddr <*> memWData) (RamRead <$> memAddr)
    (cpuRamReadData, vgaRamReadData) = withClockResetEnable clk rst enable $ memoryController cpuRamOp vgaAddr

    -- microOpQuery will be latched in microcodeRom - it must pass-through here
    microOPQuery = _microOPQuery <$> directBusOp
    microOP = withClockResetEnable clk rst enable $ microcodeRom microOPQuery

    -- VGA
    vgaDriverRes = withClockResetEnable clk rst enable $ vgaDriver vgaRamReadData
    vgaOutput = snd <$> vgaDriverRes
    vgaAddr = fst <$> vgaDriverRes

    vgaR = _vgaR <$> vgaOutput
    vgaG = _vgaG <$> vgaOutput
    vgaB = _vgaB <$> vgaOutput
    vgaHSync = toActive . _hSync <$> vgaOutput
    vgaVSync = toActive . _vSync <$> vgaOutput
    vgaClk = toActive . _blank <$> vgaOutput

makeTopEntity 'topEntity
