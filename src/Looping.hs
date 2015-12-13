-- A SpriteLike decorator for looping the sprite's frame at a certain rate
{-# LANGUAGE TypeFamilies #-}
module Looping where

import Prelude hiding (cycle)

import Control.Arrow
import Haste.Prim

import SpriteJS


data Looping a = Looping 
  { lSprite :: a
  , lCycle :: Ptr Cycle
  }

newLooping :: (CanHoldSprite p, SpriteLike a)
           => p
           -> Int  -- ^ original image width
           -> Int  -- ^ number of frames
           -> Int  -- ^ ticks per frame
           -> IO a
           -> IO (Looping a)
newLooping parent w n ticks mkSprite = do
    sprite <- mkSprite
    
    scene <- getScene parent
    cycle <- newCycle scene [(i * w, 0, ticks) | i <- [0..n-1]]
    appendToCycle cycle (rawSprite sprite)
    
    return (Looping sprite cycle)

instance SpriteLike a => SpriteLike (Looping a) where
    type UpdateParam (Looping a) = (Ptr Ticker, UpdateParam a)
    
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
    
    updateSprite (Looping sprite cycle) (ticker, x) = do
        updateSprite sprite x
        updateCycle cycle ticker
