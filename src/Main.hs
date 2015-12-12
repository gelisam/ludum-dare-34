{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.DOM
import Haste.Foreign

import SpriteJS
import WindowJS


main :: IO ()
main = do
    putStrLn "it begins!"
    
    ffi "test_game2" :: IO ()
    
    putStrLn "done."
