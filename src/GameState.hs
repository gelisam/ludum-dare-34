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
  , playerSprite :: Ptr AnimatedSprite
  , playerHeight :: Double
  , bestPlayerHeight :: Double
  , futureEntities :: [OffScreenEntity]
  , onScreenEntities :: [OnScreenEntity]
  , missedEntities :: [OffScreenEntity]
  }
