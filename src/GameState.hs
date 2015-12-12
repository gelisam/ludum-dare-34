module GameState where

import Haste.Prim

import Entities
import SpriteJS


data PlayerStatus
  = Falling
  | Floating Int -- number of balloons
  deriving (Show, Eq)


data GameState = GameState
  { playerStatus :: PlayerStatus
  , playerSprite :: Ptr Sprite
  , playerHeight :: Double
  , bestPlayerHeight :: Double
  , futureEntities :: [OffScreenEntity]
  , onScreenEntities :: [OnScreenEntity]
  , missedEntities :: [OffScreenEntity]
  }
