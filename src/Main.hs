{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow
import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Foreign
import Haste.Prim
import Text.Printf

import Animation
import AnimatedSprite
import Entities
import GameState
import JSRef
import Random
import SpriteJS
import WindowJS


game_width :: Num a => a
game_width = 640

game_height :: Num a => a
game_height = 920

gravity :: Double
gravity = 0 -- 0.5

fps :: Num a => a
fps = 25

birdPixelsPerSecond :: Num a => a
birdPixelsPerSecond = 100

playerPixelsPerSecond :: Num a => a
playerPixelsPerSecond = 100

birdScale :: Double
birdScale = 0.8

birdImageWidth :: Num a => a
birdImageWidth = 128

birdImageHeight :: Num a => a
birdImageHeight = 128

birdWidth :: Double
birdWidth = birdScale * 128

birdHeight :: Double
birdHeight = birdScale * 128


computeSeconds :: Int -> Double
computeSeconds ticks = fromIntegral ticks / fps

birdState :: OffScreenBird -> Animation (Int, Bool)
birdState = birdInitialX
      >>> flip linear birdPixelsPerSecond
      >>> bounce (birdWidth / 2, game_width - birdWidth / 2)
      >>> fmap (first floor)


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

newBirdSprite :: CanHoldSprite a => Ptr a -> IO AnimatedSprite
newBirdSprite parent = do
    sprite <- newSprite parent "img/flying-enemy.png"
    setSpriteSize sprite birdImageWidth birdImageHeight
    setSpriteScale sprite birdScale
    
    scene <- getScene parent
    cycle <- newCycle scene [(i*128, 0, 5) | i <- [0..10]]
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
    loadImages scene ["img/character.png", "img/flying-enemy.png"] $ do
      front <- newLayer scene "front"
      
      bird <- newBirdSprite front
      
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
        ticks <- getCurrentTick ticker
        let t = computeSeconds ticks
        
        let (x, isGoingLeft) = birdState (OffScreenBird 0 0) t
        let sx = if isGoingLeft then 1 else -1
        setSpritePosition (aSprite bird) (x - birdImageWidth `div` 2) (100 - birdImageHeight `div` 2)
        setSpriteXYScale (aSprite bird) (sx * birdScale) birdScale
        updateAnimatedSprite bird ticker
        
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
