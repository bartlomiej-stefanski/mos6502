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
encodeHexSevenSegment :: Unsigned 4 -> Vec 7 Bool
encodeHexSevenSegment =
  unpack . \case
    --       gfedcba
    0x0 -> 0b0111111
    0x1 -> 0b0000110
    0x2 -> 0b1011011
    0x3 -> 0b1001111
    0x4 -> 0b1100110
    0x5 -> 0b1101101
    0x6 -> 0b1111101
    0x7 -> 0b0000111
    0x8 -> 0b1111111
    0x9 -> 0b1101111
    0xa -> 0b1110111
    0xb -> 0b1111100
    0xc -> 0b0111001
    0xd -> 0b1011110
    0xe -> 0b1111001
    0xf -> 0b1110001
    _ -> error "encodeHexSevenSegment: impossible"
