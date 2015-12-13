-- A SpriteLike decorator for looping the sprite's frame at a certain rate
{-# LANGUAGE TypeFamilies #-}
module Looping where

import Prelude hiding (cycle)

import Control.Arrow
import Control.Monad
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
    forM_ (rawSprites sprite) (appendToCycle cycle)
    
    return (Looping sprite cycle)

instance SpriteLike a => SpriteLike (Looping a) where
    type UpdateParam (Looping a) = (Ptr Ticker, UpdateParam a)
    
    rawSprites       = lSprite >>> rawSprites
    collisionSprites = lSprite >>> collisionSprites
    spriteImage      = lSprite >>> spriteImage
    spriteOffset     = lSprite >>> spriteOffset
    spriteSize       = lSprite >>> spriteSize
    spriteScale      = lSprite >>> spriteScale
    spriteAngle      = lSprite >>> spriteAngle
    spriteOpacity    = lSprite >>> spriteOpacity
    
    spritePosition   = lSprite >>> spritePosition
    spriteVelocity   = lSprite >>> spriteVelocity
    
    updateSprite (Looping sprite cycle) (ticker, x) = do
        updateSprite sprite x
        updateCycle cycle ticker
