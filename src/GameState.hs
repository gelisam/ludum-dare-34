module GameState where

import Control.Arrow

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

-- move entities between the on-screen and off-screen buffers.
-- 
-- For Ordering, GT means entirely above the screen, EQ means at least partially on screen,
-- and LT means entirely below the screen.
-- 
-- Entities in the first buffer (entities below the screen) should be order from top to bottom,
-- entities in the second buffer (entities on the screen) should be ordered from bottom to top,
-- entities in the last buffer (entities above the screen) should be ordered from bottom to top.
shuffleZipper :: Monad m
              => (h -> offScreen -> Ordering)
              -> (h -> onScreen  -> Ordering)
              -> (offScreen -> m onScreen)
              -> (onScreen -> m offScreen)
              -> h
              -> ([offScreen], [onScreen], [offScreen])
              -> m ([offScreen], [onScreen], [offScreen])
shuffleZipper isOffVisible isOnVisible putOnScreen takeOffScreen h (below, current, above) = do
    newlyBelow' <- mapM takeOffScreen newlyBelow
    newlyAbove' <- mapM takeOffScreen newlyAbove
    belowCurrent' <- mapM putOnScreen belowCurrent
    aboveCurrent' <- mapM putOnScreen aboveCurrent
    
    let below'   = reverse newlyBelow' ++ stillBelow
    let current' = reverse belowCurrent' ++ stillCurrent ++ aboveCurrent'
    let above'   = newlyAbove' ++ stillAbove
    
    return (below', current', above')
  where
    (belowCurrent, stillBelow) = span (isOffVisible h >>> (/= LT)) below
    (aboveCurrent, stillAbove) = span (isOffVisible h >>> (/= GT)) above
    
    newlyBelow   = filter (isOnVisible h >>> (== LT)) current
    stillCurrent = filter (isOnVisible h >>> (== EQ)) current
    newlyAbove   = filter (isOnVisible h >>> (== GT)) current
