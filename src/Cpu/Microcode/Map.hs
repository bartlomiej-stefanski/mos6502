module Cpu.Microcode.Map where

import Clash.Prelude
import Cpu.Instructions
import Cpu.Microcode.Data

nextMicrocodeIndex :: (Instruction, AddressingMode) -> MicroOpIndex
nextMicrocodeIndex = errorX "Not implemented"
