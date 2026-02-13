module MemoryController where

import Data.Proxy
import Clash.Prelude
import Cpu.Data
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
memoryController cpuRamOp vgaOp switchInput = (cpuData, vgaData, ledData)
  where
    cpuVgaOp = getVgaQuery <$> cpuRamOp
    (cpuVgaData, vgaData) = mos6502VgaMemory cpuVgaOp vgaOp
    -- (cpuVgaData, vgaData) = (pure 0, pure 0)

    cpuCodeData = mos6502CodeRom $ getCodeRomQuery <$> cpuRamOp

    ramRead = getRamReadQuery <$> cpuRamOp
    ramWrite = getRamWriteQuery <$> cpuRamOp
    ramData = mos6502Ram ramRead ramWrite

    (updateLed, ledWrite) = unbundle $ getLedOperation <$> cpuRamOp
    ledData = regEn 0x2 updateLed ledWrite

    cpuData = chooseBusData <$> cpuRamOp <*> cpuVgaData <*> (bitCoerce <$> cpuCodeData) <*> ramData <*> switchInput
