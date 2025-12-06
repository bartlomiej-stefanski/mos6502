module Utilities.Utils where

import Clash.Prelude

type Seconds      (s :: Nat)  = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds  (1_000 * us)
type Nanoseconds  (ns :: Nat) = Picoseconds  (1_000 * ns)
type Picoseconds  (ps :: Nat) = ps

type SecondPeriods dom = Seconds 1 `Div` DomainPeriod dom
type HzToPeriod  (freq :: Nat) = Seconds 1 `Div` freq

data Polarity = High | Low

newtype Active (p :: Polarity) = MkActive{ activeLevel :: Bit }
  deriving (Show, Eq, Ord, Generic, NFDataX, BitPack)

active :: Bit -> Active p
active = MkActive

class IsActive p where
  fromActive :: Active p -> Bool
  toActive :: Bool -> Active p

instance IsActive High where
  fromActive = bitToBool . activeLevel
  toActive = MkActive . boolToBit

instance IsActive Low where
  fromActive = bitToBool . complement . activeLevel
  toActive = MkActive . complement . boolToBit
