module GameState where

import Entities
import Looping
import ScaledSprite
import SpriteJS


data PlayerStatus
  = Falling
  | Floating Int -- number of balloons
  deriving (Show, Eq)


data GameState = GameState
  { playerStatus     :: PlayerStatus
  , playerSprite     :: Looping ScaledSprite
  , gameHeight       :: Double
  , bestGameHeight   :: Double
  , futureEntities   :: [OffScreenEntity]
  , onScreenEntities :: [OnScreenEntity]
  , missedEntities   :: [OffScreenEntity]
  }
