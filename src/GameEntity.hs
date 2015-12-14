{-# LANGUAGE FunctionalDependencies #-}
module GameEntity where

import Control.Arrow

import Globals
import Constants


class GameEntity on off | on  -> off
                        , off -> on
  where
    offScreen     :: on -> off
    verticalRange :: off -> (Double, Double)
    
    putOnScreen   :: Globals -> off -> IO on
    takeOffScreen :: on -> IO off

isVisible :: GameEntity on off => Double -> off -> Ordering
isVisible screenY off
  | lo < screenY               = GT
  | hi > screenY + game_height = LT
  | otherwise                  = EQ
  where
    (lo, hi) = verticalRange off

isStillVisible :: GameEntity on off => Double -> on -> Ordering
isStillVisible screenY = offScreen >>> isVisible screenY
