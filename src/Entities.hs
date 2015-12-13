module Entities where

-- an entity is a game object the player can interact with
--   - a balloon
--   - a bird

import Control.Arrow
import Haste.Prim

import Wrapped
import Animated
import Collidable
import Centered
import Looping
import Scaled
import SpriteJS


type BalloonSprite = Wrapped (Collidable (Scaled (Centered NormalSprite)))

data OffScreenBalloon = OffScreenBalloon
  { balloonInitialX :: Double
  , balloonInitialY :: Double
  }
  deriving (Show, Eq)

data OnScreenBalloon = OnScreenBalloon
  { balloonSprite    :: BalloonSprite
  , offScreenBalloon :: OffScreenBalloon
  }

drawBalloon :: OnScreenBalloon -> UpdateParam BalloonSprite -> IO ()
drawBalloon = balloonSprite >>> updateSprite


type BirdSprite = Animated (Looping (Scaled (Centered NormalSprite)))

data OffScreenBird = OffScreenBird
  { birdInitialX :: Double
  , birdInitialY :: Double
  }
  deriving (Show, Eq)

data OnScreenBird = OnScreenBird
  { birdSprite    :: BirdSprite
  , offScreenBird :: OffScreenBird
  }

drawBird :: OnScreenBird -> UpdateParam BirdSprite -> IO ()
drawBird = birdSprite >>> updateSprite


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
    go (BalloonOn balloon) = drawBalloon balloon ()
    go (BirdOn    bird   ) = drawBird    bird    (t, h, a, (ticker, ()))
