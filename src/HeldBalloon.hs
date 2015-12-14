{-# LANGUAGE OverloadedStrings #-}
module HeldBalloon where

import Centered
import Collidable
import Constants
import JSRef
import Scaled
import SpriteJS
import Wrapped


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
