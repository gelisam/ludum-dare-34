module Backgrounds where

-- a background is a game object the player does not interact with
--   - a parallax layer
--   - a cloud

import Control.Arrow
import Haste
import Haste.Prim

import Animated
import Animation
import Centered
import SpriteJS


type ParallaxSprite = Animated NormalSprite

data OffScreenParallaxLayer = OffScreenParallaxLayer
  { parallaxImage :: JSString
  , parallaxAnim  :: Animation Double
  }

data OnScreenParallaxLayer = OnScreenParallaxLayer
  { parallaxSprite         :: ParallaxSprite
  , offScreenParallaxLayer :: OffScreenParallaxLayer
  }

drawParallaxLayer :: OnScreenParallaxLayer -> UpdateParam ParallaxSprite -> IO ()
drawParallaxLayer = parallaxSprite >>> updateSprite


type CloudSprite = Animated (Centered NormalSprite)

data OffScreenCloud = OffScreenCloud
  { cloudInitialX :: Double
  , cloudInitialY :: Double
  }
  deriving (Show, Eq)

data OnScreenCloud = OnScreenCloud
  { cloudSprite    :: CloudSprite
  , offScreenCloud :: OffScreenCloud
  }

drawCloud :: OnScreenCloud -> UpdateParam CloudSprite -> IO ()
drawCloud = cloudSprite >>> updateSprite


data OnScreenBackground
  = ParallaxOn OnScreenParallaxLayer
  | CloudOn    OnScreenCloud

data OffScreenBackground
  = ParallaxOff OffScreenParallaxLayer
  | CloudOff    OffScreenCloud


drawOnScreenBackground :: Double -> Double -> Double -> Ptr Ticker -> OnScreenBackground -> IO ()
drawOnScreenBackground t h a _ = go
  where
    go (ParallaxOn parallaxLayer) = drawParallaxLayer parallaxLayer (t, h, a, ())
    go (CloudOn    cloud        ) = drawCloud         cloud         (t, h, a, ())
