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
    directBusOp = withClockResetEnable clk rst enable $ cpuMealy (bundle (busInput, currentMicroOP))

    currentMicroOP :: Signal System MicroOP
    currentMicroOP = withClockResetEnable clk rst enable $ microcodeRom (_microOPQuery <$> directBusOp)

    memAddr = _addressToQuery <$> directBusOp
    memW = _shouldWrite <$> directBusOp
    memWData = _dataToWrite <$> directBusOp

makeTopEntity 'topEntity
