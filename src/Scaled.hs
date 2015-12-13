-- A SpriteLike decorator for scaling the original image.
-- The scaled image can be scaled further via spriteScale.
{-# LANGUAGE TypeFamilies #-}
module Scaled where

import Control.Arrow

import JSRef
import SpriteJS


data Scaled a = Scaled
  { sSprite :: a
  , sScale :: Double
  }

newScaled :: SpriteLike a
          => Double  -- ^ scale factor
          -> IO a
          -> IO (Scaled a)
newScaled scale mkSprite = do
    sprite <- mkSprite
    setSpriteScale sprite scale
    return (Scaled sprite scale)

instance SpriteLike a => SpriteLike (Scaled a) where
    type UpdateParam (Scaled a) = UpdateParam a
    
    rawSprites       = sSprite >>> rawSprites
    collisionSprites = sSprite >>> collisionSprites
    spriteImage      = sSprite >>> spriteImage
    spriteOffset     = sSprite >>> spriteOffset
    spriteSize       = sSprite >>> spriteSize
    spriteAngle      = sSprite >>> spriteAngle
    spriteOpacity    = sSprite >>> spriteOpacity
    spritePosition   = sSprite >>> spritePosition
    spriteVelocity   = sSprite >>> spriteVelocity
    updateSprite     = sSprite >>> updateSprite
    
    spriteScale (Scaled sprite scale) = JSRef
      { readJSRef  = do
          (sx,sy) <- readJSRef (spriteScale sprite)
          return (sx / scale, sy / scale)
      , writeJSRef = \(sx,sy) -> do
          writeJSRef (spriteScale sprite)
                     (sx * scale, sy * scale)
      }
