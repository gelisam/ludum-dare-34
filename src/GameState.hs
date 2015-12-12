module GameState where

import Haste.Prim

import AnimatedSprite
import Entities
import SpriteJS


data PlayerStatus
  = Falling
  | Floating Int -- number of balloons
  deriving (Show, Eq)


data GameState = GameState
  { playerStatus :: PlayerStatus
  , playerSprite :: AnimatedSprite
  , playerHeight :: Double
  , bestPlayerHeight :: Double
  , futureEntities :: [OffScreenEntity]
  , onScreenEntities :: [OnScreenEntity]
  , missedEntities :: [OffScreenEntity]
  }
