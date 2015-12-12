{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.DOM
import Haste.Foreign

import SpriteJS
import WindowJS


main :: IO ()
main = do
    putStrLn "it begins!"
    
    scene <- newScene 400 400 False
    loadImages scene ["img/bird.png"] $ do
      bird <- newSprite scene "img/bird.png"
      setSpritePosition bird 200 200
      moveSpriteBy bird (-10) (-10)
      rotateSpriteBy bird (1.0/6)
      setSpriteScale bird 1.5
      setSpriteOpacity bird 0.8
      updateSprite bird
    
    putStrLn "done."
