{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Foreign
import Haste.Prim
import Text.Printf

import JSRef
import Random
import SpriteJS
import WindowJS


game_width = 640
game_height = 920

gravity = 0.5


-- TODO: use a random balloon image instead
newBalloon :: CanHoldSprite a => Ptr a -> IO (Ptr Sprite)
newBalloon parent = do
    balloon <- newEmptySprite parent
    setSpriteSize balloon 20 20
    
    dom <- getDom balloon
    set dom [style "border" =: "2px solid #880000"]
    set dom [style "border-radius" =: "10px"]
    set dom [style "background-color" =: "red"]
    
    return balloon

main :: IO ()
main = do
    scene <- newScene game_width game_height True
    loadImages scene ["img/character.png"] $ do
      front <- newLayer scene "front"
      
      balloon <- newBalloon front
      setSpritePosition balloon 100 100
      updateSprite balloon
      
      score <- newEmptySprite front
      setSpriteSize score 200 100
      setSpritePosition score 20 20
      updateSprite score
      
      dom <- getDom score
      set dom [attr "id" =: "score"]
      
      player <- newSprite front "img/character.png"
      setSpritePosition player 40 200
      setSpriteSize player 28 52
      setSpriteXYScale player (-1) 1
      
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
        
        modifyJSRef (yVelocity player) (+ gravity)
        applyVelocity player
        
        updateSprite player
        
        updateCycle cycle ticker

        dom <- getDom score
        score_count <- readIORef score_count_ref
        set dom ["innerHTML" =: printf "Score %d" (round score_count :: Int)]
        
        y <- getSpriteY player
        when (y > game_height) $ do
          pauseTicker ticker
          alert "Game over"
        
        modifyIORef player_xv_ref (+ 0.002)
        modifyIORef score_count_ref (+ 0.08)
      runTicker ticker
