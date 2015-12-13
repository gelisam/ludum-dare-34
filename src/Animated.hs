-- A SpriteLike decorator for animating sprites according to one of three values:
--   
--   * number of seconds since the beginning of the game (not counting paused time)
--   * how high above the ground the camera is (measured in meters)
--   * how old the character is (measured in the same units as the height, except it
--     doesn't go down when the character falls)
-- 
-- Note that looped animations consisting of a sequence of frames are better handled
-- via Looped, which uses the builtin Cycle functionality from sprite.js.
{-# LANGUAGE TypeFamilies #-}
module Animated where

import Control.Arrow

import Animation
import JSRef
import SpriteJS

type Update a = a
             -> Double  -- ^ time
             -> Double  -- ^ height
             -> Double  -- ^ age
             -> IO ()

data Animated a = Animated
  { aSprite :: a
  , aUpdate :: Update a
  }

newAnimated :: SpriteLike a
          => Update a
          -> IO a
          -> IO (Animated a)
newAnimated update mkSprite = do
    sprite <- mkSprite
    update sprite 0 0 0
    return (Animated sprite update)

updatePosition :: SpriteLike a
               => Animation (Double, Double)
               -> Update a
updatePosition anim sprite t h _ = do
    let (x,y) = anim t
    let y' = h + y -- compensate for the camera's height
    writeJSRef (spritePosition sprite) (x,y')

-- True means flipped
updateDirection :: SpriteLike a
                => Animation Bool
                -> Update a
updateDirection anim sprite t _ _ = do
    let flipped = anim t
    writeJSRef (spriteXScale sprite) (if flipped then -1 else 1)

-- convenience constructor for the common case in which the position depends on the time.
-- the vertical position is measured in pixels above the ground (or rather the number of
-- pixels below the top of the first screen, since pixel.js has +Y going down), not the
-- screen.
newMoving :: SpriteLike a
          => Animation (Double, Double)
          -> IO a
          -> IO (Animated a)
newMoving anim = newAnimated $ \sprite t h a -> do
    updatePosition anim sprite t h a

-- a version of newMoving which also has a direction
newDirectionalMoving :: SpriteLike a
                     => Animation ((Double, Double), Bool)
                     -> IO a
                     -> IO (Animated a)
newDirectionalMoving anim = newAnimated $ \sprite t h a -> do
    updatePosition  (fst <$> anim) sprite t h a
    updateDirection (snd <$> anim) sprite t h a

-- convenience constructor for the common case in which the vertical position depends on
-- the height. The vertical position is measured in pixels below the top of the screen.
newParallax :: SpriteLike a
            => Animation Double
            -> IO a
            -> IO (Animated a)
newParallax anim = newAnimated $ \sprite _ h _ -> do
    let meters = h / 100  -- divide by 56 to make the building scroll at the same rate as
                          -- the entities, or by a larger number to make it scroll slower
    let y = anim meters
    writeJSRef (spriteYPosition sprite) y

instance SpriteLike a => SpriteLike (Animated a) where
    type UpdateParam (Animated a) = (Double, Double, Double, UpdateParam a)
    
    rawSprites       = aSprite >>> rawSprites
    collisionSprites = aSprite >>> collisionSprites
    spriteImage      = aSprite >>> spriteImage
    spriteOffset     = aSprite >>> spriteOffset
    spriteSize       = aSprite >>> spriteSize
    spriteScale      = aSprite >>> spriteScale
    spriteAngle      = aSprite >>> spriteAngle
    spriteOpacity    = aSprite >>> spriteOpacity
    spritePosition   = aSprite >>> spritePosition
    spriteVelocity   = aSprite >>> spriteVelocity
    applyVelocity    = aSprite >>> applyVelocity
    unapplyVelocity  = aSprite >>> unapplyVelocity
    
    updateSprite (Animated sprite update) (t, h, a, x) = do
        update sprite t h a
        updateSprite sprite x
