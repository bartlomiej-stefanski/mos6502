module VGA.ClockGenerator where

import Clash.Prelude
import Utilities.Utils

class
  ( KnownNat (HBack t),
    KnownNat (HActive t),
    KnownNat (HFront t),
    KnownNat (HSync t),
    KnownNat (VBack t),
    KnownNat (VActive t),
    KnownNat (VFront t),
    KnownNat (VSync t),
    1 <= (HActive t),
    1 <= (VActive t)
  ) =>
  VgaTiming t
  where
  type HBack t :: Nat
  type HActive t :: Nat
  type HFront t :: Nat
  type HSync t :: Nat

  type VBack t :: Nat
  type VActive t :: Nat
  type VFront t :: Nat
  type VSync t :: Nat

data Res720p

data Res1080p

-- 1280 x 720 resolution
-- 60Hz -> Pixel Clock 74.250 MHz
instance VgaTiming Res720p where
  type HBack Res720p = 220
  type HActive Res720p = 1280
  type HFront Res720p = 110
  type HSync Res720p = 40

  type VBack Res720p = 20
  type VActive Res720p = 720
  type VFront Res720p = 5
  type VSync Res720p = 5

-- 1920 x 1080 resolution
-- 30Hz -> Pixel Clock 74.250 MHz
-- 60Hz -> Pixel Clock 148.50 MHz
instance VgaTiming Res1080p where
  type HBack Res1080p = 148
  type HActive Res1080p = 1920
  type HFront Res1080p = 88
  type HSync Res1080p = 44

  type VBack Res1080p = 36
  type VActive Res1080p = 1080
  type VFront Res1080p = 4
  type VSync Res1080p = 5

data HorizontalSync t
  = HSBack (Index (HBack t))
  | HSActive (Index (HActive t))
  | HSFront (Index (HFront t))
  | HSSync (Index (HSync t))
  deriving (Eq, Show, Generic, NFDataX)

data VerticalSync t
  = VSActive (Index (VActive t))
  | VSFront (Index (VFront t))
  | VSSync (Index (VSync t))
  | VSBack (Index (VBack t))
  deriving (Eq, Show, Generic, NFDataX)

initVgaSync :: (VgaTiming t) => (HorizontalSync t, VerticalSync t)
initVgaSync = (HSActive 0, VSActive 0)

nextVgaSync ::
  (VgaTiming t) =>
  (HorizontalSync t, VerticalSync t) ->
  (HorizontalSync t, VerticalSync t)
nextVgaSync (oldHorizontal, oldVertical) = (nextHorizontal, nextVertical)
  where
    nextHorizontal = case oldHorizontal of
      HSActive x -> if x == maxBound then HSFront 0 else HSActive (x + 1)
      HSFront x -> if x == maxBound then HSSync 0 else HSFront (x + 1)
      HSSync x -> if x == maxBound then HSBack 0 else HSSync (x + 1)
      HSBack x -> if x == maxBound then HSActive 0 else HSBack (x + 1)
    horizontalFlipped = oldHorizontal == HSFront maxBound

    nextVerticalCandidate = case oldVertical of
      VSActive y -> if y == maxBound then VSFront 0 else VSActive (y + 1)
      VSFront y -> if y == maxBound then VSSync 0 else VSFront (y + 1)
      VSSync y -> if y == maxBound then VSBack 0 else VSSync (y + 1)
      VSBack y -> if y == maxBound then VSActive 0 else VSBack (y + 1)

    nextVertical = if horizontalFlipped then nextVerticalCandidate else oldVertical

data VgaSync t = VgaSync
  { _active_coords :: Maybe (Index (HActive t), Index (VActive t)),
    _h_sync :: Active High,
    _v_sync :: Active High
  }
  deriving (Eq, Show, Generic, NFDataX)

vgaClockGenerator ::
  forall dom t.
  (HiddenClockResetEnable dom, VgaTiming t) =>
  Signal dom (VgaSync t)
vgaClockGenerator = vgaSync
  where
    vgaSignalSync = register initVgaSync (nextVgaSync <$> vgaSignalSync)
    (vgaHorizontalSync, vgaVerticalSync) = unbundle vgaSignalSync

    isActive :: (HorizontalSync t, VerticalSync t) -> Maybe (Index (HActive t), Index (VActive t))
    isActive (HSActive hsActive, VSActive vsActive) = Just (hsActive, vsActive)
    isActive _ = Nothing

    isHSync :: HorizontalSync t -> Bool
    isHSync (HSSync _) = True
    isHSync _ = False

    isVSync :: VerticalSync t -> Bool
    isVSync (VSSync _) = True
    isVSync _ = False

    vgaSync =
      VgaSync
        <$> (isActive <$> vgaSignalSync)
        <*> (toActive . isHSync <$> vgaHorizontalSync)
        <*> (toActive . isVSync <$> vgaVerticalSync)
