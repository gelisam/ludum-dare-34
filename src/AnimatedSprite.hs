module AnimatedSprite where

import Haste.Prim

import ScaledSprite
import SpriteJS


data AnimatedSprite = AnimatedSprite 
  { aSprite :: ScaledSprite
  , aCycle :: Ptr Cycle
  }

newAnimatedSprite :: CanHoldSprite a
                  => a
                  -> JSString  -- ^ image file
                  -> Int  -- ^ original image width
                  -> Int  -- ^ original image height
                  -> Int  -- ^ number of frames
                  -> Int  -- ^ ticks per frame
                  -> Double  -- ^ scale factor
                  -> IO AnimatedSprite
newAnimatedSprite parent image w h n ticks scale = do
    sprite <- newScaledSprite parent image w h scale
    setSpriteSize (sSprite sprite) w h
    setSpriteScale (sSprite sprite) scale
    
    scene <- getScene parent
    cycle <- newCycle scene [(i*w, 0, ticks) | i <- [0..n-1]]
    appendToCycle cycle (sSprite sprite)
    
    return (AnimatedSprite sprite cycle)

updateAnimatedSprite :: AnimatedSprite -> Ptr Ticker -> IO ()
updateAnimatedSprite (AnimatedSprite sprite cycle) ticker = do
    updateSprite (sSprite sprite)
    updateCycle cycle ticker
