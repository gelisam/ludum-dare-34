{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Bird where

import Control.Arrow

import Animated
import Animation
import Collidable
import Constants
import Centered
import Globals
import Looping
import Scaled
import SpriteJS


type BirdSprite = Animated (Collidable (Looping (Scaled (Centered NormalSprite))))

data OffScreenBird = OffScreenBird
  { birdInitialX :: Double
  , birdInitialY :: Double
  }
  deriving (Show, Eq)

data OnScreenBird = OnScreenBird
  { birdSprite    :: BirdSprite
  , offScreenBird :: OffScreenBird
  }


birdAnimation :: OffScreenBird -> Animation ((Double, Double), Bool)
birdAnimation offScreenBird = fmap go xAndIsGoingLeft
  where
    xAndIsGoingLeft :: Animation (Double, Bool)
    xAndIsGoingLeft = bounce (birdWidth / 2, game_width - birdWidth / 2)
                    $ flip linear birdPixelsPerSecond
                    $ birdInitialX offScreenBird
    
    go :: (Double,Bool) -> ((Double, Double), Bool)
    go (x,isGoingLeft) = ((x,y),isFlipped)
      where
        y = birdInitialY offScreenBird
        isFlipped = not isGoingLeft


putBirdOnScreen :: Globals -> OffScreenBird -> IO OnScreenBird
putBirdOnScreen (Globals {..}) off = do
    sprite <- newDirectionalMoving (birdAnimation off)
            $ newCollidable globalEntityLayer (-32) (-32) 64 64
            $ newLooping globalEntityLayer birdImageWidth 11 5
            $ newScaled birdScale
            $ newCentered birdImageWidth birdImageHeight
            $ newSprite globalEntityLayer "img/flying-enemy.png"
    return (OnScreenBird sprite off)

takeBirdOffScreen :: OnScreenBird -> IO OffScreenBird
takeBirdOffScreen (OnScreenBird {..}) = do
    removeSprite birdSprite
    return offScreenBird


isBirdVisible :: Double -> OffScreenBird -> Ordering
isBirdVisible screenY (OffScreenBird _ y)
  | y + balloonHeight / 2 < screenY               = GT
  | y - balloonHeight / 2 > screenY + game_height = LT
  | otherwise                                     = EQ

isBirdStillVisible :: Double -> OnScreenBird -> Ordering
isBirdStillVisible screenY = offScreenBird >>> isBirdVisible screenY


birdYPosition :: OffScreenBird -> Double
birdYPosition = birdInitialY


drawBird :: OnScreenBird -> UpdateParam BirdSprite -> IO ()
drawBird = birdSprite >>> updateSprite
