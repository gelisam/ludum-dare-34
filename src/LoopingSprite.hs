module LoopingSprite where

import Control.Arrow
import Haste.Prim

import JSRef
import ScaledSprite
import SpriteJS


data LoopingSprite = LoopingSprite 
  { lSprite :: ScaledSprite
  , lCycle :: Ptr Cycle
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
    rawSprite       = lSprite >>> rawSprite
    spriteImage     = lSprite >>> spriteImage
    spriteSize      = lSprite >>> spriteSize
    spriteScale     = lSprite >>> spriteScale
    spriteAngle     = lSprite >>> spriteAngle
    spriteOpacity   = lSprite >>> spriteOpacity
    
    spritePosition  = lSprite >>> spritePosition
    spriteVelocity  = lSprite >>> spriteVelocity
    applyVelocity   = lSprite >>> applyVelocity
    unapplyVelocity = lSprite >>> unapplyVelocity
    
    updateSprite (LoopingSprite sprite cycle) ticker = do
        updateSprite sprite ticker
        updateCycle cycle ticker
