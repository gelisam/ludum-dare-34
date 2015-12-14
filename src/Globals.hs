{-# LANGUAGE OverloadedStrings #-}
module Globals where

import Haste.Prim

import Constants
import SpriteJS


data Globals = Globals
  { globalScene      :: Ptr Scene
  , globalBackLayer1 :: Ptr Layer
  , globalBackLayer2 :: Ptr Layer
  , globalFrontLayer :: Ptr Layer
  
  -- TODO: switch everything to the following semantic layers
  , globalSkyLayer       :: Ptr Layer
  , globalCloudLayer     :: Ptr Layer
  , globalParallaxLayer1 :: Ptr Layer
  , globalParallaxLayer2 :: Ptr Layer
  , globalParallaxLayer3 :: Ptr Layer
  , globalEntityLayer    :: Ptr Layer
  , globalMessages       :: Ptr Layer
  }

newGlobals :: IO Globals
newGlobals = do
    scene <- newScene game_width game_height True
    back2 <- newLayer scene "back-2"
    back1 <- newLayer scene "back-1"
    front <- newLayer scene "front"
    
    skyLayer       <- newLayer scene "skyLayer"
    cloudLayer     <- newLayer scene "cloudLayer"
    parallaxLayer1 <- newLayer scene "parallaxLayer1"
    parallaxLayer2 <- newLayer scene "parallaxLayer2"
    parallaxLayer3 <- newLayer scene "parallaxLayer3"
    entityLayer    <- newLayer scene "entityLayer"
    messages       <- newLayer scene "messages"
    
    return $ Globals
      { globalScene      = scene
      
      , globalBackLayer1 = back1
      , globalBackLayer2 = back2
      , globalFrontLayer = front
      
      , globalSkyLayer       = skyLayer
      , globalCloudLayer     = cloudLayer
      , globalParallaxLayer1 = parallaxLayer1
      , globalParallaxLayer2 = parallaxLayer2
      , globalParallaxLayer3 = parallaxLayer3
      , globalEntityLayer    = entityLayer
      , globalMessages       = messages
      }
