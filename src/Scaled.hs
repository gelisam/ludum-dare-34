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
-- that we can draw the image around the center point set by setScaledPosition:
-- 
--   ...........
--   ...........
--   ....***....
--   ....*+*....
--   ....***....
--   ...........
--   ...........
{-# LANGUAGE TypeFamilies #-}
module Scaled where

import Control.Arrow

import JSRef
import SpriteJS


data Scaled a = Scaled
  { initialImageWidth  :: Int
  , initialImageHeight :: Int
  , sSprite :: a
  , sScale :: Double
  }

newScaled :: SpriteLike a
          => Int  -- ^ original image width
          -> Int  -- ^ original image height
          -> Double  -- ^ scale factor
          -> IO a
          -> IO (Scaled a)
newScaled w h scale mkSprite = do
    sprite <- mkSprite
    writeJSRef (spriteSize sprite) (fromIntegral w, fromIntegral h)
    setSpriteScale sprite scale
    return (Scaled w h sprite scale)

instance SpriteLike a => SpriteLike (Scaled a) where
    type UpdateParam (Scaled a) = UpdateParam a
    
    rawSprite = sSprite >>> rawSprite
    
    spriteSize (Scaled w h _ _) = JSRef
      { readJSRef  = return (fromIntegral w, fromIntegral h)
      , writeJSRef = \_ -> error "bad idea, the dimentions stored in Scaled would no longer match"
      }
    spriteScale (Scaled _ _ sprite scale) = JSRef
      { readJSRef  = do
          (sx,sy) <- readJSRef (spriteScale sprite)
          return (sx / scale, sy / scale)
      , writeJSRef = \(sx,sy) -> do
          writeJSRef (spriteScale sprite)
                     (sx * scale, sy * scale)
      }
    spritePosition (Scaled w h sprite _) = JSRef
      { readJSRef  = do
          (x,y) <- readJSRef (spritePosition sprite)
          return (x + fromIntegral w / 2, y + fromIntegral h / 2)
      , writeJSRef = \(x,y) -> do
          writeJSRef (spritePosition sprite)
                     (x - fromIntegral w / 2, y - fromIntegral h / 2)
      }
