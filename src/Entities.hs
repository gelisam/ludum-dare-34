{-# LANGUAGE MultiParamTypeClasses #-}
module Entities where

-- an entity is a game object the player can interact with
--   - a balloon
--   - a bird

import Haste.Prim

import Balloon
import Bird
import GameObject
import Globals
import SpriteJS


data OnScreenEntity
  = BalloonOn OnScreenBalloon
  | BirdOn    OnScreenBird

data OffScreenEntity
  = BalloonOff OffScreenBalloon
  | BirdOff   OffScreenBird
  deriving (Show, Eq)

instance GameObject OnScreenEntity OffScreenEntity where
    offScreenObject (BalloonOn b) = BalloonOff $ offScreenObject b
    offScreenObject (BirdOn    b) = BirdOff    $ offScreenObject b
    
    objectYPosition (BalloonOff b) = objectYPosition b
    objectYPosition (BirdOff    b) = objectYPosition b
    
    objectHeight (BalloonOff b) = objectHeight b
    objectHeight (BirdOff    b) = objectHeight b

    putOnScreen g (BalloonOff b) = BalloonOn <$> putOnScreen g b
    putOnScreen g (BirdOff    b) = BirdOn    <$> putOnScreen g b
    
    takeOffScreen (BalloonOn b) = BalloonOff <$> takeOffScreen b
    takeOffScreen (BirdOn    b) = BirdOff    <$> takeOffScreen b


drawOnScreenEntity :: Double -> Double -> Double -> Ptr Ticker -> OnScreenEntity -> IO ()
drawOnScreenEntity t h a ticker = go
  where
    go (BalloonOn balloon) = drawBalloon balloon (t, h, a, ())
    go (BirdOn    bird   ) = drawBird    bird    (t, h, a, (ticker, ()))
