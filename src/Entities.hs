module Entities where

import Haste.Prim

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
  { balloonSprite :: Ptr Sprite
  , offScreenBalloon :: OffScreenBalloon
  }


data OffScreenBird = OffScreenBird
  { birdInitialX :: Double
  , birdHeight :: Double
  }
  deriving (Show, Eq)

data OnScreenBird = OnScreenBird
  { birdSprite :: Ptr Sprite
  , offScreenBird :: OffScreenBird
  }


data OnScreenEntity
  = OnScreenBalloonEntity OnScreenBalloon
  | OnScreenBirEntityd OnScreenBird

data OffScreenEntity
  = OffScreenBalloonEntity OffScreenBalloon
  | OffScreenBirdEntity OffScreenBird
  deriving (Show, Eq)
