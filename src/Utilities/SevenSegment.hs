module Utilities.SevenSegment where

import Clash.Prelude

-- | Encodes a hexadecimal digit (0-F) to a 7-segment display with a pattern:
--
--      aaaa
--     f    b
--     f    b
--      gggg
--     e    c
--     e    c
--      dddd
--
encodeHexSevenSegment :: Unsigned 4 -> Vec 7 Bool
encodeHexSevenSegment = unpack . \case
  --       abcdefg
  0x0 -> 0b1111110
  0x1 -> 0b0110000
  0x2 -> 0b1101101
  0x3 -> 0b1111001
  0x4 -> 0b0110011
  0x5 -> 0b1011011
  0x6 -> 0b1011111
  0x7 -> 0b1110000
  0x8 -> 0b1111111
  0x9 -> 0b1111011
  0xa -> 0b1110111
  0xb -> 0b0011111
  0xc -> 0b1001110
  0xd -> 0b0111101
  0xe -> 0b1001111
  0xf -> 0b1000111
  _   -> error "encodeHexSevenSegment: impossible"
