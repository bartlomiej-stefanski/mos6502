{-# OPTIONS_GHC -freduction-depth=0 #-}

module Cpu.Microcode.Rom where

import Clash.Prelude
import Cpu.Microcode.Data
import Cpu.Microcode.Map

opcodeMapperRom :: Vec 256 MicroOPRomAddress
opcodeMapperRom = $(listToVecTH addrLookupList)

microcodeRom :: (HiddenClockResetEnable dom) => Signal dom MicroOPRomAddress -> Signal dom MicroOP
microcodeRom addr = rom $(listToVecTH rawMicrocodeList) (bitCoerce <$> addr)
