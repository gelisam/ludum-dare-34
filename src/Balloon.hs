{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Balloon where

import Control.Arrow

import Animated
import Collidable
import Constants
import Centered
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


putBalloonOnScreen :: Globals -> OffScreenBalloon -> IO OnScreenBalloon
putBalloonOnScreen (Globals {..}) (off@(OffScreenBalloon x y)) = do
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

takeBalloonOffScreen :: OnScreenBalloon -> IO OffScreenBalloon
takeBalloonOffScreen (OnScreenBalloon {..}) = do
    removeSprite balloonSprite
    return offScreenBalloon


isBalloonVisible :: Double -> OffScreenBalloon -> Ordering
isBalloonVisible screenY (OffScreenBalloon _ y)
  | y + balloonHeight / 2 < screenY               = GT
  | y - balloonHeight / 2 > screenY + game_height = LT
  | otherwise                                     = EQ

isBalloonStillVisible :: Double -> OnScreenBalloon -> Ordering
isBalloonStillVisible screenY = offScreenBalloon >>> isBalloonVisible screenY


balloonYPosition :: OffScreenBalloon -> Double
balloonYPosition = balloonInitialY


drawBalloon :: OnScreenBalloon -> UpdateParam BalloonSprite -> IO ()
drawBalloon = balloonSprite >>> updateSprite
