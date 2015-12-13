-- A SpriteLike decorator for wrapping the sprite around
-- when the sprite touches the sides of the screen.
-- 
-- Only horizontal wrapping is supported.
{-# LANGUAGE TypeFamilies #-}
module Wrapped where

import Data.Fixed
import Data.List
import Data.Ord

import JSRef
import SpriteJS


-- we need two sprites because when the Wrapped sprite is touching the
-- screen, we need to draw one copy on the left of the screen and another
-- on the right.
data Wrapped a = Wrapped 
  { wSprite1 :: a
  , wSprite2 :: a
  , wrappedScreenWidth :: Int
  }

newWrapped :: SpriteLike a
           => Int  -- ^ screen width
           -> IO a
           -> IO (Wrapped a)
newWrapped screenWidth mkSprite = do
    sprite1 <- mkSprite
    sprite2 <- mkSprite
    return (Wrapped sprite1 sprite2 screenWidth)

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


instance SpriteLike a => SpriteLike (Wrapped a) where
    type UpdateParam (Wrapped a) = UpdateParam a
    
    rawSprites       (Wrapped s1 s2 _) = rawSprites       s1 ++ rawSprites       s2
    collisionSprites (Wrapped s1 s2 _) = collisionSprites s1 ++ collisionSprites s2
    
    spriteImage    (Wrapped s1 s2 _) = syncedJSRefs (spriteImage    s1) (spriteImage    s2)
    spriteOffset   (Wrapped s1 s2 _) = syncedJSRefs (spriteOffset   s1) (spriteOffset   s2)
    spriteSize     (Wrapped s1 s2 _) = syncedJSRefs (spriteSize     s1) (spriteSize     s2)
    spriteScale    (Wrapped s1 s2 _) = syncedJSRefs (spriteScale    s1) (spriteScale    s2)
    spriteAngle    (Wrapped s1 s2 _) = syncedJSRefs (spriteAngle    s1) (spriteAngle    s2)
    spriteOpacity  (Wrapped s1 s2 _) = syncedJSRefs (spriteOpacity  s1) (spriteOpacity  s2)
    spriteVelocity (Wrapped s1 s2 _) = syncedJSRefs (spriteVelocity s1) (spriteVelocity s2)
    
    updateSprite    (Wrapped s1 s2 _) x = do
        updateSprite s1 x
        updateSprite s2 x
    
    spritePosition (Wrapped s1 s2 screenWidth) = JSRef
      { readJSRef  = do
          (x,y) <- readJSRef (spritePosition s1)
          return (innerXPosition screenWidth x, y)
      , writeJSRef = \(x,y) -> do
          writeJSRef (spritePosition s1) (innerXPosition screenWidth x, y)
          writeJSRef (spritePosition s2) (outerXPosition screenWidth x, y)
      }
