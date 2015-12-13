module Entities where

import SpriteJS


-- an entity is anything the player can interact with
--   - a balloon
--   - a bird


data OffScreenBalloon = OffScreenBalloon
  { balloonX :: Double
  , balloonHeight :: Double
  }
  deriving (Show, Eq)

data OnScreenBalloon = OnScreenBalloon
  { balloonSprite    :: NormalSprite
  , offScreenBalloon :: OffScreenBalloon
  }


data OffScreenBird = OffScreenBird
  { birdInitialX :: Double
  , birdInitialY :: Double
  }
  deriving (Show, Eq)

data OnScreenBird = OnScreenBird
  { birdSprite    :: NormalSprite
  , offScreenBird :: OffScreenBird
  }


data OnScreenEntity
  = OnScreenBalloonEntity OnScreenBalloon
  | OnScreenBirEntityd OnScreenBird

data OffScreenEntity
  = OffScreenBalloonEntity OffScreenBalloon
  | OffScreenBirdEntity OffScreenBird
  deriving (Show, Eq)
