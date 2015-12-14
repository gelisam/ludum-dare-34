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
  }

newGlobals :: IO Globals
newGlobals = do
    scene <- newScene game_width game_height True
    back2 <- newLayer scene "back-2"
    back1 <- newLayer scene "back-1"
    front <- newLayer scene "front"
    
    return $ Globals
      { globalScene      = scene
      , globalBackLayer1 = back1
      , globalBackLayer2 = back2
      , globalFrontLayer = front
      }
