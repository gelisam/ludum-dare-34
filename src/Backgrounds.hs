module Backgrounds where

-- a background is a game object the player does not interact with
--   - a parallax layer
--   - a cloud

import Haste

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


data OnScreenBackground
  = ParallaxOn OnScreenParallaxLayer
  | CloudOn    OnScreenCloud

data OffScreenBackground
  = ParallaxOff OffScreenParallaxLayer
  | CloudOff    OffScreenCloud
