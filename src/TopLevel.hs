module TopLevel where

import Clash.Annotations.TH
import Clash.Prelude
import Utilities.SevenSegment
import Utilities.Utils

topEntity ::
  "SWITCHES" ::: Signal System (Vec 4 (Active High)) ->
  "LEDS" ::: Signal System (Vec 7 (Active Low))
topEntity switches = bitCoerce . encodeHexSevenSegment . bitCoerce <$> switches

makeTopEntity 'topEntity
