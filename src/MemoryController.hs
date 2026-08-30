module MemoryController where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Data
import Utilities.Utils
import VgaDriver

type RomSize = 0x2000

type RomAddr = Unsigned (Log2 RomSize)

codeRomFile :: FilePath
codeRomFile = "programs/.build/apps/vga_hello/vga_hello.bin.txt"

mos6502CodeRom ::
  (HiddenClockResetEnable dom) =>
  Signal dom RomAddr ->
  Signal dom Data
mos6502CodeRom addr = bitCoerce <$> romFile (SNat @RomSize) codeRomFile addr

type RamSize = 0x8000

type RamAddr = Unsigned (Log2 RamSize)

emptyInitRam :: Vec RamSize Data
emptyInitRam = repeat (errorX "RAM does not have a starting value")

mos6502Ram ::
  (HiddenClockResetEnable dom) =>
  Signal dom RamAddr ->
  Signal dom (Maybe (RamAddr, Data)) ->
  Signal dom Data
mos6502Ram = blockRamU
  NoClearOnReset
  (SNat @RamSize)
  (const (errorX "Odcytano niezainicjowany adres RAM"))

data MemoryDevice
  = DevRAM
  | DevVGA
  | DevROM
  | DevNone
  deriving (Show, Eq, Generic, NFDataX)

decodeDevice :: Addr -> MemoryDevice
decodeDevice addr
  | addr < 0x8000 = DevRAM
  | addr >= 0x8000 && addr < 0xE000 = DevVGA
  | addr >= 0xE000 = DevROM
  | otherwise = DevNone

memoryController ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  "ADDR_QUERY" ::: Signal System Addr ->
  "CPU_DATA_OUT" ::: Signal System Data ->
  "CPU_DATA_W" ::: Signal System (Active High) ->
  ( "CPU_DATA_IN" ::: Signal System Data,
    -- VGA video buffer is managed externally.
    "VGA_ADDR" ::: Signal System VgaAddr,
    "VGA_DATA_OUT" ::: Signal System Data,
    "VGA_DATA_W" ::: Signal System (Active High)
  )
memoryController clk rst enable addr_query cpu_data_out cpu_data_w =
  (cpu_data_in, vga_addr, cpu_data_out, vga_data_w)
  where
    dev_query = decodeDevice <$> addr_query

    -- RAM
    ram_addr = resize <$> addr_query
    ram_op =
      (\dev addr data_w -> if dev == DevRAM then Just (addr, data_w) else Nothing)
        <$> dev_query
        <*> ram_addr
        <*> cpu_data_out
    ram = withClockResetEnable clk rst enable $ mos6502Ram ram_addr ram_op

    -- ROM
    rom_addr = resize <$> addr_query
    code_rom = withClockResetEnable clk rst enable $ mos6502CodeRom rom_addr

    -- VGA
    vga_addr = resize <$> addr_query
    vga_data_w =
      (\dev data_w -> if dev == DevVGA then data_w else toActive False)
        <$> dev_query
        <*> cpu_data_w

    dev_query_delayed = withClockResetEnable clk rst enable $
                            register DevNone dev_query

    -- Choose data to feed to CPU
    mux_data :: MemoryDevice -> Data -> Data -> Data
    mux_data DevRAM ram_data _ = ram_data
    mux_data DevROM _ rom_data = rom_data
    mux_data DevVGA _ _ = errorX "Can't read data from VGA video BUS"
    mux_data _ _ _ = errorX "Read from invalid device"

    cpu_data_in = mux_data <$> dev_query_delayed <*> ram <*> code_rom

makeTopEntity 'memoryController
