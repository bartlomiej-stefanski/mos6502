{-# OPTIONS_GHC -freduction-depth=0 #-}

module Cpu.Microcode.Rom where

import Clash.Prelude
import Cpu.Microcode.Data
import Cpu.Microcode.Map

microcodeRom :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 10) -> Signal dom MicroOP
microcodeRom =
  rom
    $(listToVecTH rawMicrocodeList)
