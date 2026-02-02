module Cpu.Data where

import Clash.Prelude

type Data = Unsigned 8

type Addr = Unsigned 16

splitAddr :: Addr -> (Data, Data)
splitAddr addr = (highByte, lowByte)
  where
    highByte = bitCoerce $ slice d15 d8 addr :: Data
    lowByte = bitCoerce $ slice d7 d0 addr :: Data
