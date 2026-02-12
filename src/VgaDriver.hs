module VgaDriver where

import Clash.Prelude
import Cpu.Data
import Data.Proxy

-- Timing for VGA 640x480, 60Hz
type HVisible = 640

type HFront = 16

type HSync = 96

type HBack = 48

type HTotal = HVisible + HFront + HSync + HBack

type VVisible = 480

type VFront = 10

type VSync = 2

type VBack = 33

type VTotal = VVisible + VFront + VSync + VBack

type VgaRamSize = 8192

type VgaAddr = Unsigned (CLog 2 VgaRamSize)

hVisible :: VgaAddr
hVisible = fromIntegral $ natVal (Proxy :: Proxy HVisible)

vVisible :: VgaAddr
vVisible = fromIntegral $ natVal (Proxy :: Proxy VVisible)

lettersPerRow :: VgaAddr
lettersPerRow = hVisible `div` 8

vgaBufferStart :: Addr
vgaBufferStart = 0x6000

vgaBufferEnd :: Addr
vgaBufferEnd = vgaBufferStart + (fromInteger $ natVal (Proxy :: Proxy VgaRamSize))

data HorizontalSync
  = HSVisible (Index HVisible)
  | HSFront (Index HFront)
  | HSSync (Index HSync)
  | HSBack (Index HBack)
  deriving (Eq, Show, Generic, NFDataX)

data VerticalSync
  = VSVisible (Index VVisible)
  | VSFront (Index VFront)
  | VSSync (Index VSync)
  | VSBack (Index VBack)
  deriving (Eq, Show, Generic, NFDataX)

initVgaSync :: (HorizontalSync, VerticalSync)
initVgaSync = (HSVisible 0, VSVisible 0)

nextVgaSync :: (HorizontalSync, VerticalSync) -> (HorizontalSync, VerticalSync)
nextVgaSync (oldHorizontal, oldVertical) = (nextHorizontal, nextVertical)
  where
    nextHorizontal = case oldHorizontal of
      HSVisible x -> if x == maxBound then HSFront 0 else HSVisible (x + 1)
      HSFront x -> if x == maxBound then HSSync 0 else HSFront (x + 1)
      HSSync x -> if x == maxBound then HSBack 0 else HSSync (x + 1)
      HSBack x -> if x == maxBound then HSVisible 0 else HSBack (x + 1)
    horizontalFlipped = oldHorizontal == HSBack maxBound

    nextVertical = case oldVertical of
      VSVisible y -> if y == maxBound && horizontalFlipped then VSFront 0 else VSVisible (y + 1)
      VSFront y -> if y == maxBound && horizontalFlipped then VSSync 0 else VSFront (y + 1)
      VSSync y -> if y == maxBound && horizontalFlipped then VSBack 0 else VSSync (y + 1)
      VSBack y -> if y == maxBound && horizontalFlipped then VSVisible 0 else VSBack (y + 1)

data VgaOutput = VgaOutput
  { _vgaR :: Data,
    _vgaG :: Data,
    _vgaB :: Data,
    _hSync :: Bool,
    _vSync :: Bool,
    _blank :: Bool
  }
  deriving (Eq, Show, Generic, NFDataX)

delayEn2 ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  a ->
  Signal dom Bool ->
  Signal dom a ->
  Signal dom a
delayEn2 d en = regEn d en . regEn d en

-- | The rom of 8x8 characters. Chosen so the n-th character is then n-th ASCII char.
charRomFilePath :: FilePath
charRomFilePath = "roms/font8x8rom.bin"

charRom :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 11) -> Signal dom (BitVector 8)
charRom = romFile (pow2SNat d11) charRomFilePath

getCharLine :: Unsigned 3 -> Data -> Unsigned 11
getCharLine line char = bitCoerce (char, line)

type VgaMemoryOp = RamOp VgaRamSize Data

vgaDriver ::
  (HiddenClockResetEnable dom) =>
  Signal dom Data ->
  Signal dom (VgaMemoryOp, VgaOutput)
vgaDriver ramDataIn = bundle (RamRead <$> ramReadAddr, VgaOutput <$> vgaR <*> vgaG <*> vgaB <*> hSyncOut <*> vSyncOut <*> pixelEn)
  where
    -- Board runs on 50MHz clock, this downscales it to 25MHz for VGA timing
    clockEn = register False (not <$> clockEn)

    vgaSync = regEn initVgaSync clockEn (nextVgaSync <$> vgaSync)
    vgaHorizontalSync = fst <$> vgaSync
    vgaVerticalSync = snd <$> vgaSync

    getX :: HorizontalSync -> VgaAddr
    getX = \case
      HSVisible x -> fromIntegral $ x
      _ -> 0

    getY :: VerticalSync -> VgaAddr
    getY = \case
      VSVisible y -> fromIntegral $ y
      _ -> 0

    getPixelEn :: (HorizontalSync, VerticalSync) -> Bool
    getPixelEn = \case
      (HSVisible _, VSVisible _) -> True
      _ -> False

    getHSync :: HorizontalSync -> Bool
    getHSync = \case
      HSSync _ -> True
      _ -> False

    getVSync :: VerticalSync -> Bool
    getVSync = \case
      VSSync _ -> True
      _ -> False

    currX = getX <$> vgaHorizontalSync
    currY = getY <$> vgaVerticalSync

    getHighAddr :: VgaAddr -> VgaAddr
    getHighAddr addr = zeroExtend $ unpack $ slice d7 d3 addr
    getLowAddr :: VgaAddr -> Unsigned 3
    getLowAddr addr = unpack $ slice d2 d0 addr

    -- Address of the current character in the VGA buffer.
    ramReadAddr = bitCoerce <$> (getHighAddr <$> currX) + ((*) lettersPerRow <$> (getHighAddr <$> currY))

    -- Delay lowY by one cycle - untill CharRom query.
    lowY = regEn 0 clockEn (getLowAddr <$> currY)
    charRomData = charRom $ getCharLine <$> lowY <*> ramDataIn

    -- Delay sync, and pixel data by two cycles - untill we have charRomData ready.
    hSyncOut = delayEn2 False clockEn (getHSync <$> vgaHorizontalSync)
    vSyncOut = delayEn2 False clockEn (getVSync <$> vgaVerticalSync)
    pixelEn = delayEn2 False clockEn (getPixelEn <$> vgaSync)
    lowX = delayEn2 0 clockEn (getLowAddr <$> currX)

    currPixel = (.==.) 1 $ (!) <$> charRomData <*> lowX
    lightBit = mux currPixel 255 0

    vgaR = mux pixelEn lightBit 0
    vgaG = mux pixelEn lightBit 0
    vgaB = mux pixelEn lightBit 0
