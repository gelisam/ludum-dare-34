{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RecordWildCards #-}
module Balloon where

import Control.Arrow

import Animated
import Collidable
import Constants
import Centered
import GameObject
import Globals
import JSRef
import Random
import Scaled
import SpriteJS
import Wrapped


type BalloonSprite = Animated (Wrapped (Collidable (Scaled (Centered NormalSprite))))

data OffScreenBalloon = OffScreenBalloon
  { balloonInitialX :: Double
  , balloonInitialY :: Double
  }
  deriving (Show, Eq)

data OnScreenBalloon = OnScreenBalloon
  { balloonSprite    :: BalloonSprite
  , offScreenBalloon :: OffScreenBalloon
  }


instance GameObject OnScreenBalloon OffScreenBalloon where
    offScreenObject = offScreenBalloon
    objectYPosition = balloonInitialY
    objectHeight _  = balloonHeight

    putOnScreen (Globals {..}) (off@(OffScreenBalloon x y)) = do
        sprite <- newMoving (\t -> (x + 5 * cos t, y))
                $ newWrapped game_width
                $ newCollidable globalEntityLayer (-30) (-70) 50 50
                $ newScaled balloonScale
                $ newCentered balloonImageWidth balloonImageHeight
                $ newSprite globalEntityLayer "img/balloons.png"
        r <- randomRIO (0,3)
        let offset = balloonImageHeight * (r :: Int)
        writeJSRef (spriteYOffset sprite) (fromIntegral offset)
        return (OnScreenBalloon sprite off)
    takeOffScreen (OnScreenBalloon {..}) = do
        removeSprite balloonSprite
        return offScreenBalloon


drawBalloon :: OnScreenBalloon -> UpdateParam BalloonSprite -> IO ()
drawBalloon = balloonSprite >>> updateSprite
