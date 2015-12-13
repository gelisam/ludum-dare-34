-- A SpriteLike decorator tweaking spritePosition to mean
-- the center of the image instead of the top-left corner.
{-# LANGUAGE TypeFamilies #-}
module Centered where

-- sprite.js supports scaling sprites, but the way in which the sprites
-- are centered is inconsistent with the way they are positioned. Suppose
-- you draw a square sprite:
-- 
--   ...........
--   ...+****...
--   ...*****...
--   ...*****...
--   ...*****...
--   ...*****...
--   ...........
-- 
-- The "+" indicates the position given to the sprite via spritePosition.
-- Next, suppose we scale down the sprite via spriteScale:
-- 
--   ...........
--   ...+.......
--   ....***....
--   ....***....
--   ....***....
--   ...........
--   ...........
-- 
-- The position is now neither on the top-left nor in the center of the sprite!
-- In this module I implement sprites which know their initial image size, so
-- that we can draw the image around the center point set by spritePosition:
-- 
--   ...........
--   ...........
--   ....***....
--   ....*+*....
--   ....***....
--   ...........
--   ...........

import Control.Arrow

import JSRef
import SpriteJS


data Centered a = Centered
  { cSprite :: a
  , initialImageWidth  :: Int
  , initialImageHeight :: Int
  }

newCentered :: SpriteLike a
            => Int  -- ^ original image width
            -> Int  -- ^ original image height
            -> IO a
            -> IO (Centered a)
newCentered w h mkSprite = do
    sprite <- mkSprite
    writeJSRef (spriteSize sprite) (fromIntegral w, fromIntegral h)
    return (Centered sprite w h)

-- helper for non-centered sprites to also set their size
newTopLeftAligned :: SpriteLike a
                  => Int  -- ^ original image width
                  -> Int  -- ^ original image height
                  -> IO a
                  -> IO a
newTopLeftAligned w h mkSprite = do
    sprite <- mkSprite
    writeJSRef (spriteSize sprite) (fromIntegral w, fromIntegral h)
    return sprite

instance SpriteLike a => SpriteLike (Centered a) where
    type UpdateParam (Centered a) = UpdateParam a
    
    rawSprites       = cSprite >>> rawSprites
    collisionSprites = cSprite >>> collisionSprites
    spriteImage      = cSprite >>> spriteImage
    spriteOffset     = cSprite >>> spriteOffset
    spriteScale      = cSprite >>> spriteScale
    spriteAngle      = cSprite >>> spriteAngle
    spriteOpacity    = cSprite >>> spriteOpacity
    spriteVelocity   = cSprite >>> spriteVelocity
    updateSprite     = cSprite >>> updateSprite
    
    spriteSize (Centered _ w h) = JSRef
      { readJSRef  = return (fromIntegral w, fromIntegral h)
      , writeJSRef = \_ -> error "bad idea, the dimentions stored in Centered would no longer match"
      }
    spritePosition (Centered sprite w h) = JSRef
      { readJSRef  = do
          (x,y) <- readJSRef (spritePosition sprite)
          return (x + fromIntegral w / 2, y + fromIntegral h / 2)
      , writeJSRef = \(x,y) -> do
          writeJSRef (spritePosition sprite)
                     (x - fromIntegral w / 2, y - fromIntegral h / 2)
      }
