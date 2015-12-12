-- sprite.js supports scaling sprites, but the way in which the sprites
-- are scaled is inconsistent with the way they are positioned. Suppose
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
-- The "+" indicates the position given to the sprite via setSpritePosition.
-- Next, suppose we scale down the sprite using setSpriteScale:
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
-- that we can draw the image around the center point set by setScaledSpritePosition:
-- 
--   ...........
--   ...........
--   ....***....
--   ....*+*....
--   ....***....
--   ...........
--   ...........
module ScaledSprite where

import Haste.Prim

import SpriteJS


data ScaledSprite = ScaledSprite
  { initialImageWidth  :: Int
  , initialImageHeight :: Int
  , sSprite :: Ptr Sprite
  , sScale :: Double
  }

newScaledSprite :: CanHoldSprite a
                => Ptr a
                -> JSString
                -> Int -> Int
                -> Double
                -> IO ScaledSprite
newScaledSprite parent image w h scale = do
    sprite <- newSprite parent image
    setSpriteSize sprite w h
    setSpriteScale sprite scale
    return (ScaledSprite w h sprite scale)

setScaledSpritePosition :: ScaledSprite -> Double -> Double -> IO ()
setScaledSpritePosition (ScaledSprite w h sprite _) x y = setSpritePosition sprite x' y'
  where
    x' = round x - w `div` 2
    y' = round y - h `div` 2

setScaledSpriteXYScale :: ScaledSprite -> Double -> Double -> IO ()
setScaledSpriteXYScale (ScaledSprite _ _ sprite scale) sx sy = setSpriteXYScale sprite sx' sy'
  where
    sx' = scale * sx
    sy' = scale * sy

setScaledSpriteScale :: ScaledSprite -> Double -> IO ()
setScaledSpriteScale sprite scale = setScaledSpriteXYScale sprite scale scale
