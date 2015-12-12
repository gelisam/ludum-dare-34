{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.DOM
import Haste.Foreign

import SpriteJS
import WindowJS


main :: IO ()
main = do
    putStrLn "it begins!"
    
    let game_height = 480
    let game_width = 640
    scene <- newScene game_width game_height True
    loadImages scene ["img/character.png", "img/crate.png"] $ do
      back <- newLayer scene "backround"
      front <- newLayer scene "front"
      
      score <- newEmptySprite front
      setSpriteSize score 200 100
      setSpritePosition score 20 20
      updateSprite score
      
      dom <- getDom score
      set dom [attr "id" =: "score"]
      
      rest game_width game_height scene back front score
    
    putStrLn "done."
