module VgaDriver where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu.Data
import Utilities.Utils
import VGA.ClockGenerator
import VGA.PixelVgaDriver

type VgaAddr = Unsigned 13

-- Transforms 8bit color to 24bit color.
pixelRenderer :: Data -> Unsigned 24
pixelRenderer color = bitCoerce (red, repeat @5 red_low, green, repeat @5 green_low, blue, repeat @6 blue_low)
  where
    red = slice d7 d5 color
    red_low = slice d5 d5 color

    green = slice d4 d2 color
    green_low = slice d2 d2 color

    blue = slice d1 d0 color
    blue_low = slice d0 d0 color

vgaDriver ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  "BUFFER_DATA" ::: Signal System Data ->
  ( "BUFFER_QUERY" ::: Signal System VgaAddr,
    "VGA_DATA" ::: Signal System (Unsigned 24),
    "VGA_ACTIVE" ::: Signal System (Active High),
    "VGA_H_SYNC" ::: Signal System (Active High),
    "VGA_V_SYNC" ::: Signal System (Active High)
  )
vgaDriver clk rst enable buffer_data = (bufferQuery, vgaOutput, output_active, hsync, vsync)
  where
    (vga_out, vga_query) = withClockResetEnable clk rst enable $ pixelVgaDriver @Res720p @4 0 pixelRenderer buffer_data

    get_query_addr :: VgaAddrQuery Res720p 4 -> Unsigned 13
    get_query_addr (Just (h_index, v_index)) = bitCoerce (v_index, h_index)
    get_query_addr Nothing = 0

    bufferQuery = get_query_addr <$> vga_query
    vgaOutput = _pixel_data <$> vga_out
    output_active = _output_active <$> vga_out
    hsync = _vga_hsync <$> vga_out
    vsync = _vga_vsync <$> vga_out

makeTopEntity 'vgaDriver
