module TopLevel where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Cpu
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
    "MEM_W_DATA" ::: Signal System Data
  )
topEntity clk rst enable busInput = (memAddr, memW, memWData)
  where
    directBusOp = withClockResetEnable clk rst enable $ cpuMealy (bundle (busInput, microOP))

    -- directBusOp is combinational circuit output -> it must be latched to guarantee stability
    memAddr = withClockResetEnable clk rst enable $ register 0 (_addressToQuery <$> directBusOp)
    memW = withClockResetEnable clk rst enable $ register (toActive False) (_shouldWrite <$> directBusOp)
    memWData = withClockResetEnable clk rst enable $ register 0 (_dataToWrite <$> directBusOp)

    -- microOpQuery will be latched in microcodeRom - it must pass-through here
    microOPQuery = _microOPQuery <$> directBusOp

    microOP :: Signal System MicroOP
    microOP = withClockResetEnable clk rst enable $ microcodeRom microOPQuery

makeTopEntity 'topEntity
