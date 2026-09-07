module VGA.PixelVgaDriver where

import Clash.Prelude
import Data.Maybe
import Utilities.Utils
import VGA.ClockGenerator

type VgaAddrQuery t scale = Maybe (Index (Div (HActive t) (2 ^ scale)), Index (Div (VActive t) (2 ^ scale)))

data VgaOut = VgaOut
  { _pixel_data :: Unsigned 24,
    _output_active :: Active High,
    _vga_hsync :: Active High,
    _vga_vsync :: Active High
  }
  deriving (Eq, Show, Generic, NFDataX)

pixelVgaDriver ::
  forall t scale pixel dom.
  ( HiddenClockResetEnable dom,
    VgaTiming t,
    KnownNat scale,
    NFDataX pixel,
    1 <= Div (HActive t) (2 ^ scale),
    1 <= Div (VActive t) (2 ^ scale)
  ) =>
  pixel ->
  (pixel -> Unsigned 24) ->
  Signal dom pixel ->
  ( Signal dom VgaOut,
    Signal dom (VgaAddrQuery t scale)
  )
pixelVgaDriver blank_pixel pixel_renderer buffer_data = (vga_out, buffer_query)
  where
    timings :: Signal dom (VgaSync t)
    timings = vgaClockGenerator

    scaleVal :: Int
    scaleVal = snatToNum (SNat @scale)

    calcQuery :: VgaSync t -> VgaAddrQuery t scale
    calcQuery vgaSync = case _active_coords vgaSync of
      (Just (x, y)) ->
        Just
          ( fromIntegral (x `shiftR` scaleVal),
            fromIntegral (y `shiftR` scaleVal)
          )
      _ -> Nothing

    buffer_query = calcQuery <$> timings

    -- Memory reads are delayed by one cycle - output must be delayed as well
    generate_vga_out :: VgaSync t -> VgaOut
    generate_vga_out vga_sync = VgaOut (pixel_renderer blank_pixel) (toActive . isJust . _active_coords $ vga_sync) (_h_sync vga_sync) (_v_sync vga_sync)

    initVgaOut = VgaOut (pixel_renderer blank_pixel) (toActive False) (toActive False) (toActive False)

    next_output = register initVgaOut (generate_vga_out <$> timings)
    asked_previously = register False (isJust <$> buffer_query)

    vga_out =
      (\asked next_out in_data -> if asked then next_out {_pixel_data = pixel_renderer in_data} else next_out)
        <$> asked_previously
        <*> next_output
        <*> buffer_data
