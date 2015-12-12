{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.DOM
import Haste.Foreign

import SpriteJS


visual_guide :: IO ()
visual_guide = do
    -- create the DOM element expected by visual_guide.js
    div <- newElem "ol" `with` [ attr "id"        =: "exec_list"
                               , style "position" =: "absolute"
                               , style "left"     =: "400px"
                               ]
    appendChild documentBody div
    
    ffi "visual_guide"


console_log :: JSString -> IO ()
console_log = ffi "(function(x) {console.log(x)})"

main :: IO ()
main = do
    putStrLn "it begins!"
    
    setDebug True
    w <- windowWidth
    h <- windowHeight
    scene <- newScene w h False
    loadImages scene ["ground.png", "tiles.png"] $ do
      setMainCallback scene $ do
        surface <- newScrollingSurface scene
        updateSurface surface
      loadMap scene "map.json"
    
    -- visual_guide  -- sprites but no tiles
    console_log "done."
