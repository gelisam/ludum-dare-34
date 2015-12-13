module GameState where

import Entities
import Looping
import Centered
import Scaled
import SpriteJS


data PlayerStatus
  = Falling
  | Floating Int -- number of balloons
  deriving (Show, Eq)


data GameState = GameState
  { playerStatus     :: PlayerStatus
  , playerSprite     :: Looping (Scaled (Centered NormalSprite))
  , gameHeight       :: Double
  , bestGameHeight   :: Double
  , futureEntities   :: [OffScreenEntity]
  , onScreenEntities :: [OnScreenEntity]
  , missedEntities   :: [OffScreenEntity]
  }
