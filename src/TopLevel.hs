module TopLevel where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Cpu
import Cpu.Data
import Cpu.Microcode.Rom
import Utilities.Utils
import Utilities.SevenSegment
import VgaDriver
import MemoryController
import Distribution.Simple (LowerBound)

topEntity ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  "SWITCHES" ::: Signal System Data ->
  "BUTTON" ::: Signal System (Active High) ->
  ( "VGA_R" ::: Signal System Data,
    "VGA_G" ::: Signal System Data,
    "VGA_B" ::: Signal System Data,
    "VGA_HSYNC" ::: Signal System (Active High),
    "VGA_VSYNC" ::: Signal System (Active High),
    "VGA_BLANK_N" ::: Signal System (Active Low),
    "LEDS" ::: Signal System Data,
    "SEG0" ::: Signal System (Vec 7 (Active Low)),
    "SEG1" ::: Signal System (Vec 7 (Active Low))
  )
topEntity clk rst enable switches button = (vgaR, vgaG, vgaB, vgaHSync, vgaVSync, vgaClk, ledData, sig0, sig1)
  where
    directBusOp = withClockResetEnable clk rst enable $ cpuMealy (bundle (cpuRamReadData, microOP))

    memAddr = bitCoerce . _addressToQuery <$> directBusOp
    memW = _shouldWrite <$> directBusOp
    memWData = _dataToWrite <$> directBusOp

    -- Ram operation will be latched in ram - it must pass-through here
    cpuRamOp :: Signal System (RamOp AddressSpace Data)
    cpuRamOp = mux (fromActive <$> memW) (RamWrite <$> memAddr <*> memWData) (RamRead <$> memAddr)
    (cpuRamReadData, vgaRamReadData, ledData, segData) =
      withClockResetEnable clk rst enable $ memoryController cpuRamOp vgaAddr switches button

    -- microOpQuery will be latched in microcodeRom - it must pass-through here
    microOPQuery = _microOPQuery <$> directBusOp
    microOP = withClockResetEnable clk rst enable $ microcodeRom microOPQuery

    sig0 = fmap toActive <$> encodeHexSevenSegment . bitCoerce <$> slice d3 d0 <$> segData
    sig1 = fmap toActive <$> encodeHexSevenSegment . bitCoerce <$> slice d7 d4 <$> segData

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
