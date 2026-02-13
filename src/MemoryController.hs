module MemoryController where

import Data.Proxy
import Clash.Prelude
import Cpu.Data
import VgaDriver
import Utilities.Utils

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
switchesAddress = 0x4002

buttonAddress :: Addr
buttonAddress = 0x4003

ledAddress :: Addr
ledAddress = 0x4000

segAddress :: Addr
segAddress = 0x4001

getPortOperation :: Addr -> CpuMemoryOp -> (Bool, Data)
getPortOperation portAddr (RamWrite addr dat) = (addr == (bitCoerce portAddr), dat)
getPortOperation _ _ = (False, 0)

chooseBusData :: CpuMemoryOp -> Data -> Data -> Data -> Data -> Active High -> Data
chooseBusData (RamRead addr) vgaData codeData ramData switchData button = case addr of
  _ | addr >= (bitCoerce romAddressStart) -> codeData
  _ | addr < (bitCoerce ramSize) -> ramData
  _ | addr == (bitCoerce switchesAddress) -> switchData
  _ | addr == (bitCoerce buttonAddress) -> if (fromActive button) then 1 else 0
  _ -> vgaData
chooseBusData _ _ _ _ _ _ = 0

memoryController ::
  (HiddenClockResetEnable dom) =>
  Signal dom CpuMemoryOp ->
  Signal dom VgaMemoryOp ->
  Signal dom Data ->
  Signal dom (Active High) ->
  (Signal dom Data, Signal dom Data, Signal dom Data, Signal dom Data)
memoryController cpuRamOp vgaOp switchInput button = (cpuData, vgaData, ledData, segData)
  where
    cpuVgaOp = getVgaQuery <$> cpuRamOp
    (cpuVgaData, vgaData) = mos6502VgaMemory cpuVgaOp vgaOp

    cpuCodeData = mos6502CodeRom $ getCodeRomQuery <$> cpuRamOp

    ramRead = getRamReadQuery <$> cpuRamOp
    ramWrite = getRamWriteQuery <$> cpuRamOp
    ramData = mos6502Ram ramRead ramWrite

    (updateLed, ledWrite) = unbundle $ getPortOperation ledAddress <$> cpuRamOp
    ledData = regEn 0 updateLed ledWrite

    (updateSeg, segWrite) = unbundle $ getPortOperation segAddress <$> cpuRamOp
    segData = regEn 0x11 updateSeg segWrite

    delayChoseBusData = register (RamRead 0 :: CpuMemoryOp) cpuRamOp
    cpuData = chooseBusData <$> delayChoseBusData <*> cpuVgaData <*> (bitCoerce <$> cpuCodeData) <*> ramData <*> switchInput <*> button
