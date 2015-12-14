module Entities where

-- an entity is a game object the player can interact with
--   - a balloon
--   - a bird

import Haste.Prim

import Balloon
import Bird
import SpriteJS


data OnScreenEntity
  = BalloonOn OnScreenBalloon
  | BirdOn    OnScreenBird

data OffScreenEntity
  = BaloonOff OffScreenBalloon
  | BirdOff   OffScreenBird
  deriving (Show, Eq)


drawOnScreenEntity :: Double -> Double -> Double -> Ptr Ticker -> OnScreenEntity -> IO ()
drawOnScreenEntity t h a ticker = go
  where
    go (BalloonOn balloon) = drawBalloon balloon (t, h, a, ())
    go (BirdOn    bird   ) = drawBird    bird    (t, h, a, (ticker, ()))
