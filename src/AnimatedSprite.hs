module AnimatedSprite where

import Haste.Prim

import SpriteJS


data AnimatedSprite = AnimatedSprite 
  { aSprite :: Ptr Sprite
  , aCycle :: Ptr Cycle
  }

updateAnimatedSprite :: AnimatedSprite -> Ptr Ticker -> IO ()
updateAnimatedSprite (AnimatedSprite sprite cycle) ticker = do
    updateSprite sprite
    updateCycle cycle ticker
