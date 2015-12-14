module Entities where

-- an entity is a game object the player can interact with
--   - a balloon
--   - a bird

import Haste.Prim

import Balloon
import Bird
import Globals
import SpriteJS


data OnScreenEntity
  = BalloonOn OnScreenBalloon
  | BirdOn    OnScreenBird

data OffScreenEntity
  = BalloonOff OffScreenBalloon
  | BirdOff   OffScreenBird
  deriving (Show, Eq)

offScreenEntity :: OnScreenEntity -> OffScreenEntity
offScreenEntity (BalloonOn b) = BalloonOff $ offScreenBalloon b
offScreenEntity (BirdOn    b) = BirdOff    $ offScreenBird    b

putEntityOnScreen :: Globals -> OffScreenEntity -> IO OnScreenEntity
putEntityOnScreen g (BalloonOff b) = BalloonOn <$> putBalloonOnScreen g b
putEntityOnScreen g (BirdOff    b) = BirdOn    <$> putBirdOnScreen    g b

takeEntityOffScreen :: OnScreenEntity -> IO OffScreenEntity
takeEntityOffScreen (BalloonOn b) = BalloonOff <$> takeBalloonOffScreen b
takeEntityOffScreen (BirdOn    b) = BirdOff    <$> takeBirdOffScreen    b


isEntityVisible :: Double -> OffScreenEntity -> Ordering
isEntityVisible screenY (BalloonOff b) = isBalloonVisible screenY b
isEntityVisible screenY (BirdOff    b) = isBirdVisible    screenY b

isEntityStillVisible :: Double -> OnScreenEntity -> Ordering
isEntityStillVisible screenY (BalloonOn b) = isBalloonStillVisible screenY b
isEntityStillVisible screenY (BirdOn    b) = isBirdStillVisible    screenY b


drawOnScreenEntity :: Double -> Double -> Double -> Ptr Ticker -> OnScreenEntity -> IO ()
drawOnScreenEntity t h a ticker = go
  where
    go (BalloonOn balloon) = drawBalloon balloon (t, h, a, ())
    go (BirdOn    bird   ) = drawBird    bird    (t, h, a, (ticker, ()))
