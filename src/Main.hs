{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Foreign
import Haste.Prim
import Text.Printf

import AnimatedSprite
import Entities
import GameState
import JSRef
import Random
import SpriteJS
import WindowJS


game_width = 640
game_height = 920

gravity = 0.5

fps = 25
birdPixelsPerSecond = 100
playerPixelsPerSecond = 100


newPlayerSprite :: CanHoldSprite a => Ptr a -> IO AnimatedSprite
newPlayerSprite parent = do
      sprite <- newSprite parent "img/character.png"
      setSpriteSize sprite 28 52
      setSpriteXYScale sprite (-1) 1
      
      scene <- getScene parent
      cycle <- newCycle scene [ (3, 3, 5)
                              , (33, 3, 5)
                              , (63, 3, 5)
                              , (93, 3, 5)
                              , (123, 3, 5)
                              , (153, 3, 5)
                              , (183, 3, 5)
                              ]
      appendToCycle cycle sprite
      
      return (AnimatedSprite sprite cycle)

-- TODO: use a random balloon image instead
newBalloonSprite :: CanHoldSprite a => Ptr a -> IO (Ptr Sprite)
newBalloonSprite parent = do
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
      
      balloon <- newBalloonSprite front
      setSpritePosition balloon 100 100
      updateSprite balloon
      
      score <- newEmptySprite front
      setSpriteSize score 200 100
      setSpritePosition score 20 20
      updateSprite score
      
      dom <- getDom score
      set dom [attr "id" =: "score"]
      
      player <- newPlayerSprite front
      setSpritePosition (aSprite player) 40 200
      
      player_xv_ref <- newIORef 2.5
      score_count_ref <- newIORef 0.0
      
      ticker <- newTicker scene fps $ \ticker -> do
        player_xv <- readIORef player_xv_ref
        score_count <- readIORef score_count_ref
        
        modifyJSRef (yVelocity (aSprite player)) (+ gravity)
        applyVelocity (aSprite player)
        
        updateAnimatedSprite player ticker

        dom <- getDom score
        score_count <- readIORef score_count_ref
        set dom ["innerHTML" =: printf "Score %d" (round score_count :: Int)]
        
        y <- getSpriteY (aSprite player)
        when (y > game_height) $ do
          pauseTicker ticker
          alert "Game over"
        
        modifyIORef player_xv_ref (+ 0.002)
        modifyIORef score_count_ref (+ 0.08)
      runTicker ticker
