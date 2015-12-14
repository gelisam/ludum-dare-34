{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
module Balloons where

import Constants
import Wrapped
import Collidable
import Centered
import SpriteJS
import Scaled
import JSRef

type HeldBalloonSprite = Wrapped (Collidable (Scaled (Centered NormalSprite)))

newHeldBalloonSprite :: CanHoldSprite a => a -> Int -> IO HeldBalloonSprite
newHeldBalloonSprite parent color = do
    balloon <- newWrapped game_width
             $ newCollidable parent (-30) (-70) 50 50
             $ newScaled balloonScale
             $ newCentered balloonImageWidth balloonImageHeight
             $ newSprite parent "img/balloons.png"
    let offset = 475 * color
    writeJSRef (spriteYOffset balloon) (fromIntegral offset)
    return balloon
