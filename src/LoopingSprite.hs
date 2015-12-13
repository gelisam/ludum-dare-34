module LoopingSprite where

import Control.Arrow
import Haste.Prim

import JSRef
import ScaledSprite
import SpriteJS


data LoopingSprite = LoopingSprite 
  { aSprite :: ScaledSprite
  , aCycle :: Ptr Cycle
  }

newLoopingSprite :: CanHoldSprite a
                  => a
                  -> JSString  -- ^ image file
                  -> Int  -- ^ original image width
                  -> Int  -- ^ original image height
                  -> Int  -- ^ number of frames
                  -> Int  -- ^ ticks per frame
                  -> Double  -- ^ scale factor
                  -> IO LoopingSprite
newLoopingSprite parent image w h n ticks scale = do
    sprite <- newScaledSprite parent image w h scale
    
    scene <- getScene parent
    cycle <- newCycle scene [(i * w, 0, ticks) | i <- [0..n-1]]
    appendToCycle cycle (sSprite sprite)
    
    return (LoopingSprite sprite cycle)

instance SpriteLike LoopingSprite where
    rawSprite       = aSprite >>> rawSprite
    spriteImage     = aSprite >>> spriteImage
    spriteSize      = aSprite >>> spriteSize
    spriteScale     = aSprite >>> spriteScale
    spriteAngle     = aSprite >>> spriteAngle
    spriteOpacity   = aSprite >>> spriteOpacity
    
    spritePosition  = aSprite >>> spritePosition
    spriteVelocity  = aSprite >>> spriteVelocity
    applyVelocity   = aSprite >>> applyVelocity
    unapplyVelocity = aSprite >>> unapplyVelocity
    
    updateSprite (LoopingSprite sprite cycle) ticker = do
        updateSprite sprite ticker
        updateCycle cycle ticker
