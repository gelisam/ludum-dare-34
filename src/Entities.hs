{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Entities where

-- an entity is a game object the player can interact with
--   - a balloon
--   - a bird

import Control.Arrow
import Haste.Prim

import Animated
import Collidable
import Constants
import Centered
import Globals
import JSRef
import Looping
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
    sprite <- newMoving (pure (x,y))
            $ newWrapped game_width
            $ newCollidable globalEntityLayer (-30) (-70) 50 50
            $ newScaled balloonScale
            $ newCentered balloonImageWidth balloonImageHeight
            $ newSprite globalEntityLayer "img/balloons.png"
    r <- randomRIO (0,3)
    let offset = balloonImageHeight * (r :: Int)
    writeJSRef (spriteYOffset sprite) (fromIntegral offset)
    return (OnScreenBalloon sprite off)

drawBalloon :: OnScreenBalloon -> UpdateParam BalloonSprite -> IO ()
drawBalloon = balloonSprite >>> updateSprite


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

drawBird :: OnScreenBird -> UpdateParam BirdSprite -> IO ()
drawBird = birdSprite >>> updateSprite


data OnScreenEntity
  = BalloonOn OnScreenBalloon
  | BirdOn    OnScreenBird

data OffScreenEntity
  = BaloonOff OffScreenBalloon
  | BirdOff   OffScreenBird
  deriving (Show, Eq)


drawOnScreenEntity :: Double -> Double -> Double -> Ptr Ticker -> OnScreenEntity -> IO ()
drawOnScreenEntity t h a ticker = go
  where
    go (BalloonOn balloon) = drawBalloon balloon (t, h, a, ())
    go (BirdOn    bird   ) = drawBird    bird    (t, h, a, (ticker, ()))
