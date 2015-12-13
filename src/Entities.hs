module Entities where

-- an entity is a game object the player can interact with
--   - a balloon
--   - a bird

import Animated
import Centered
import Looping
import Scaled
import SpriteJS


type BalloonSprite = Animated (Centered NormalSprite)

data OffScreenBalloon = OffScreenBalloon
  { balloonInitialX :: Double
  , balloonInitialY :: Double
  }
  deriving (Show, Eq)

data OnScreenBalloon = OnScreenBalloon
  { balloonSprite    :: BalloonSprite
  , offScreenBalloon :: OffScreenBalloon
  }


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


data OnScreenEntity
  = BalloonOn OnScreenBalloon
  | BirdOn    OnScreenBird

data OffScreenEntity
  = BaloonOff OffScreenBalloon
  | BirdOff   OffScreenBird
  deriving (Show, Eq)
