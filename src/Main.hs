{-# LANGUAGE OverloadedStrings #-}
import Data.IORef
import Haste
import Haste.DOM
import Haste.Foreign

import JSRef
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
      
      bottom <- newSprite back "img/crate.png"
      setSpriteSize bottom game_width 64
      setSpritePosition bottom 0 (game_height-64)
      updateSprite bottom
      
      elements <- newSpriteList
      appendToSpriteList elements bottom
      
      player <- newSprite front "img/character.png"
      setSpritePosition player 40 200
      setSpriteSize player 28 52
      setSpriteXYScale player (-1) 1
      
      input <- newInput scene
      
      cycle <- newCycle scene [ (3, 3, 5)
                              , (33, 3, 5)
                              , (63, 3, 5)
                              , (93, 3, 5)
                              , (123, 3, 5)
                              , (153, 3, 5)
                              , (183, 3, 5)
                              ]
      appendToCycle cycle player
      
      player_xv_ref <- newIORef 2.5
      score_count_ref <- newIORef 0.0
      
      ticker <- newTicker scene 25 $ \ticker -> do
        player_xv <- readIORef player_xv_ref
        score_count <- readIORef score_count_ref
        
        let gravity = 0.5
        modifyJSRef (yVelocity player) (+ gravity)
        applyXVelocity player
        
        r <- collidesWithSpriteList player elements
        case r of
          Just otherSprite -> do
            pauseTicker ticker
            alert "Game over!"
          Nothing -> do
            applyYVelocity player
            
            r <- collidesWithSpriteList player elements
            case r of
              Just otherSprite -> do
                unapplyYVelocity player
                writeJSRef (yVelocity player) 0
                -- if(input.mousedown || input.keydown) {                                                                           \
                modifyJSRef (yVelocity player) (subtract 10)
              Nothing -> do
                return ()
                -- if(input.mousedown || input.keydown) {                                                                           \
                modifyJSRef (yVelocity player) (subtract 0.2)
            updateSprite player
            
            rest game_width game_height scene back front score bottom elements player input cycle player_xv score_count ticker
            
            modifyIORef player_xv_ref (+ 0.002)
            modifyIORef score_count_ref (+ 0.08)
      runTicker ticker
    
    putStrLn "done."
