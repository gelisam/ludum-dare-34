-- A SpriteLike decorator for selecting a subset of the image bounds
-- as its collision box.
-- 
-- Only orthogonal collision boxes are supported.
{-# LANGUAGE TypeFamilies #-}
module Collidable where

import Control.Arrow
import Control.Monad
import Data.Fixed
import Data.List
import Data.Ord
import Haste.DOM

import JSRef
import SpriteJS


-- set to True to make the collision boxes visible
debugCollisionBoxes :: Bool
debugCollisionBoxes = False


-- we need two sprites because when the Collidable sprite is touching the
-- screen, we need to draw one copy on the left of the screen and another
-- on the right.
data Collidable a = Collidable 
  { cSprite          :: a
  , cCollisionSprite :: NormalSprite
  , cXOffset         :: Int
  , cYOffset         :: Int
  }

newCollisionSprite :: CanHoldSprite parent
                   => parent
                   -> Int  -- ^ width
                   -> Int  -- ^ height
                   -> IO NormalSprite
newCollisionSprite parent w h = do
    sprite <- newEmptySprite parent
    writeJSRef (spriteSize sprite) (fromIntegral w, fromIntegral h)
    
    when debugCollisionBoxes $ do
      dom <- getDom sprite
      set dom [style "border" =: "2px solid green"]
    
    return sprite

newCollidable :: (CanHoldSprite parent, SpriteLike a)
              => parent
              -> Int  -- ^ offset from x position
              -> Int  -- ^ offset from y position
              -> Int  -- ^ width
              -> Int  -- ^ height
              -> IO a
              -> IO (Collidable a)
newCollidable parent dx dy w h mkSprite = do
    sprite <- mkSprite
    collisionSprite <- newCollisionSprite parent w h
    return (Collidable sprite collisionSprite dx dy)

syncedJSRefs :: JSRef a -> JSRef a -> JSRef a
syncedJSRefs ref1 ref2 = JSRef
  { readJSRef  = readJSRef ref1
  , writeJSRef = \x -> do
      writeJSRef ref1 x
      writeJSRef ref2 x
  }


-- each position is conceptually repeated every screenWidth units.
-- "inner" is the one closest to the center of the screen, and
-- "outer" is the second closest. Those will necessarily be on
-- opposite sides of the screen and they're the only copies we need.

distanceFromScreenCenter :: Int -> Double -> Double
distanceFromScreenCenter screenWidth x = abs (x - fromIntegral screenWidth / 2)

innerAndOuterXPositions :: Int -> Double -> [Double]
innerAndOuterXPositions screenWidth x = sortBy (comparing $ distanceFromScreenCenter screenWidth)
                                      $ [ x' - fromIntegral screenWidth
                                        , x'
                                        , x' + fromIntegral screenWidth
                                        ]
  where
    x' = x `mod'` fromIntegral screenWidth

innerXPosition :: Int -> Double -> Double
innerXPosition screenWidth x = innerAndOuterXPositions screenWidth x !! 0

outerXPosition :: Int -> Double -> Double
outerXPosition screenWidth x = innerAndOuterXPositions screenWidth x !! 1


instance SpriteLike a => SpriteLike (Collidable a) where
    type UpdateParam (Collidable a) = UpdateParam a
    
    rawSprites       = cSprite >>> rawSprites
    spriteImage      = cSprite >>> spriteImage
    spriteOffset     = cSprite >>> spriteOffset
    spriteSize       = cSprite >>> spriteSize
    spriteScale      = cSprite >>> spriteScale
    spriteOpacity    = cSprite >>> spriteOpacity
    spriteVelocity   = cSprite >>> spriteVelocity
    removeSprite     (Collidable s1 s2 _ _) = do
      removeSprite s1
      removeSprite s2

    collisionSprites = cCollisionSprite >>> collisionSprites
    
    spriteAngle      = error "bad idea, the collision box would not follow"
    
    updateSprite    (Collidable s1 s2 _ _) x = do
        updateSprite s1 x
        updateSprite s2 ()
    
    spritePosition (Collidable s1 s2 dx dy) = JSRef
      { readJSRef  = readJSRef (spritePosition s1)
      , writeJSRef = \(x,y) -> do
          writeJSRef (spritePosition s1) (x                  , y                  )
          writeJSRef (spritePosition s2) (x + fromIntegral dx, y + fromIntegral dy)
      }
