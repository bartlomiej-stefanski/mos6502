module TopLevel where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Cpu
import Cpu.Data
import Cpu.Microcode.Rom
import Data.Proxy
import Utilities.Utils
import VgaDriver

type CpuMemoryOp = RamOp AddressSpace Data

romAddressStart :: Addr
romAddressStart = 0x8000

codeRomFile :: FilePath
codeRomFile = "roms/code.bin"

mos6502CodeRom ::
  (HiddenClockResetEnable dom) =>
  Signal dom (Unsigned 15) ->
  Signal dom (BitVector 8)
mos6502CodeRom = romFile (pow2SNat d15) codeRomFile

getCodeRomQuery :: CpuMemoryOp -> Unsigned 15
getCodeRomQuery (RamRead addr) = bitCoerce $ slice d14 d0 addr
getCodeRomQuery _ = 0

mos6502VgaMemory ::
  (HiddenClockResetEnable dom) =>
  Signal dom (RamOp VgaRamSize Data) ->
  Signal dom (RamOp VgaRamSize Data) ->
  (Signal dom Data, Signal dom Data)
mos6502VgaMemory = trueDualPortBlockRam

getVgaQuery :: CpuMemoryOp -> RamOp VgaRamSize Data
getVgaQuery (RamRead addr) = RamRead (bitCoerce (slice d12 d0 addr))
getVgaQuery (RamWrite addr dat) =
  if addr >= (bitCoerce vgaBufferStart) && addr < (bitCoerce vgaBufferEnd)
    then RamWrite (bitCoerce (slice d12 d0 addr)) dat
    else RamRead 0
getVgaQuery _ = RamRead 0

type RamSize = 4096

type RamAddr = Unsigned (CLog 2 RamSize)

ramSize :: Addr
ramSize = fromIntegral (natVal (Proxy :: Proxy RamSize))

emptyInitRam :: Vec RamSize Data
emptyInitRam = repeat 0

mos6502Ram ::
  (HiddenClockResetEnable dom) =>
  Signal dom RamAddr ->
  Signal dom (Maybe (RamAddr, Data)) ->
  Signal dom Data
mos6502Ram = blockRam emptyInitRam

getRamReadQuery :: CpuMemoryOp -> RamAddr
getRamReadQuery (RamRead addr) = bitCoerce $ slice d11 d0 addr
getRamReadQuery _ = 0

getRamWriteQuery :: CpuMemoryOp -> Maybe (RamAddr, Data)
getRamWriteQuery (RamWrite addr dat) =
  if addr < (bitCoerce ramSize)
    then Just (bitCoerce $ slice d11 d0 addr, dat)
    else Nothing
getRamWriteQuery _ = Nothing

switchesAddress :: Addr
switchesAddress = 0x4000

ledAddress :: Addr
ledAddress = 0x4001

getLedOperation :: CpuMemoryOp -> (Bool, Data)
getLedOperation (RamWrite addr dat) = (addr == (bitCoerce ledAddress), dat)
getLedOperation _ = (False, 0)

chooseBusData :: CpuMemoryOp -> Data -> Data -> Data -> Data -> Data
chooseBusData (RamRead addr) vgaData codeData ramData switchData = case addr of
  _ | addr >= (bitCoerce romAddressStart) -> codeData
  _ | addr < (bitCoerce ramSize) -> ramData
  _ | addr == (bitCoerce switchesAddress) -> switchData
  _ -> vgaData
chooseBusData _ _ _ _ _ = 0

memoryController ::
  (HiddenClockResetEnable dom) =>
  Signal dom CpuMemoryOp ->
  Signal dom VgaMemoryOp ->
  Signal dom Data ->
  (Signal dom Data, Signal dom Data, Signal dom Data)
memoryController nextCpuRamOp nextVgaOp switchInput = (cpuData, vgaData, ledData)
  where
    -- Bus operations must be latched to avoid combinational loop with memory!
    cpuRamOp = register (RamRead 0) nextCpuRamOp
    vgaOp = register (RamRead 0) nextVgaOp

    cpuVgaOp = getVgaQuery <$> cpuRamOp
    (cpuVgaData, vgaData) = mos6502VgaMemory cpuVgaOp vgaOp
    -- (cpuVgaData, vgaData) = (pure 0, pure 0)

    cpuCodeData = mos6502CodeRom $ getCodeRomQuery <$> cpuRamOp

    ramRead = getRamReadQuery <$> cpuRamOp
    ramWrite = getRamWriteQuery <$> cpuRamOp
    ramData = mos6502Ram ramRead ramWrite

    ledData = regEn 0 updateLed ledWrite
    (updateLed, ledWrite) = unbundle $ getLedOperation <$> cpuRamOp

    cpuData = chooseBusData <$> cpuRamOp <*> cpuVgaData <*> (bitCoerce <$> cpuCodeData) <*> ramData <*> switchInput

topEntity ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  "SWITCHES" ::: Signal System Data ->
  ( "VGA_R" ::: Signal System Data,
    "VGA_G" ::: Signal System Data,
    "VGA_B" ::: Signal System Data,
    "VGA_HSYNC" ::: Signal System (Active High),
    "VGA_VSYNC" ::: Signal System (Active High),
    "VGA_BLANK_N" ::: Signal System (Active Low),
    "LEDS" ::: Signal System Data
  )
topEntity clk rst enable switches = (vgaR, vgaG, vgaB, vgaHSync, vgaVSync, vgaClk, ledData)
  where
    -- topEntity clk rst enable switches = (pure 0, pure 0, pure 0, pure $ toActive True, pure $ toActive True, pure $ toActive True, ledData)

    directBusOp = withClockResetEnable clk rst enable $ cpuMealy (bundle (cpuRamReadData, microOP))

    -- directBusOp is combinational circuit output -> it must be latched to guarantee stability
    memAddr = bitCoerce <$> _addressToQuery <$> directBusOp :: Signal System (Index AddressSpace)
    memW = _shouldWrite <$> directBusOp
    memWData = _dataToWrite <$> directBusOp

    -- Ram operation will be latched in ram - it must pass-through here
    cpuRamOp :: Signal System (RamOp AddressSpace Data)
    cpuRamOp = mux (fromActive <$> memW) (RamWrite <$> memAddr <*> memWData) (RamRead <$> memAddr)
    (cpuRamReadData, vgaRamReadData, ledData) = withClockResetEnable clk rst enable $ memoryController cpuRamOp vgaAddr switches
    -- (cpuRamReadData, _, ledData) = withClockResetEnable clk rst enable $ memoryController cpuRamOp (pure $ RamRead 0) switches

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
