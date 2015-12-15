{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module GameObject where

import Control.Arrow

import Globals
import Constants


-- only supports centered objects
class GameObject on off | on  -> off
                        , off -> on
  where
    offScreenObject :: on -> off
    objectYPosition :: off -> Double
    objectHeight    :: off -> Double
    
    putOnScreen   :: Globals -> off -> IO on
    takeOffScreen :: on -> IO off

verticalRange :: GameObject on off => off -> (Double, Double)
verticalRange off = (y + h / 2, y - h / 2)
  where
    y = objectYPosition off
    h = objectHeight    off

isVisible :: GameObject on off => Double -> off -> Ordering
isVisible screenY off
  | lo < screenY               = GT
  | hi > screenY + game_height = LT
  | otherwise                  = EQ
  where
    (lo, hi) = verticalRange off

isStillVisible :: GameObject on off => Double -> on -> Ordering
isStillVisible screenY = offScreenObject >>> isVisible screenY
