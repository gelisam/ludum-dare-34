{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RecordWildCards #-}
module Bird where

import Control.Arrow

import Animated
import Animation
import Collidable
import Constants
import Centered
import GameObject
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


instance GameObject OnScreenBird OffScreenBird where
    offScreenObject = offScreenBird
    objectYPosition = birdInitialY
    objectHeight _  = birdHeight

    putOnScreen (Globals {..}) off = do
        sprite <- newDirectionalMoving (birdAnimation off)
                $ newCollidable globalEntityLayer (-32) (-32) 64 64
                $ newLooping globalEntityLayer birdImageWidth 11 5
                $ newScaled birdScale
                $ newCentered birdImageWidth birdImageHeight
                $ newSprite globalEntityLayer "img/flying-enemy.png"
        return (OnScreenBird sprite off)
    takeOffScreen (OnScreenBird {..}) = do
        removeSprite birdSprite
        return offScreenBird


drawBird :: OnScreenBird -> UpdateParam BirdSprite -> IO ()
drawBird = birdSprite >>> updateSprite
