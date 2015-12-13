{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow
import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Foreign
import Text.Printf

import Animation
import Entities
import GameState
import JSRef
import Looping
import Random
import Scaled
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

playerPixelsPerSecond :: Num a => a
playerPixelsPerSecond = 100

playerScale :: Double
playerScale = 1

playerImageWidth :: Num a => a
playerImageWidth = 30

playerImageHeight :: Num a => a
playerImageHeight = 52

playerWidth :: Double
playerWidth = playerScale * playerImageWidth

playerHeight :: Double
playerHeight = playerScale * playerImageHeight


birdPixelsPerSecond :: Num a => a
birdPixelsPerSecond = 100

birdScale :: Double
birdScale = 0.8

birdImageWidth :: Num a => a
birdImageWidth = 128

birdImageHeight :: Num a => a
birdImageHeight = 128

birdWidth :: Double
birdWidth = birdScale * birdImageWidth

birdHeight :: Double
birdHeight = birdScale * birdImageHeight


computeSeconds :: Int -> Double
computeSeconds ticks = fromIntegral ticks / fps

birdState :: OffScreenBird -> Animation (Double, Bool)
birdState = birdInitialX
        >>> flip linear birdPixelsPerSecond
        >>> bounce (birdWidth / 2, game_width - birdWidth / 2)


newPlayerSprite :: CanHoldSprite a => a -> IO (Looping (Scaled NormalSprite))
newPlayerSprite parent = do
    sprite <- newLooping parent playerImageWidth 7 5
            $ newScaled 30 52 1.0
            $ newSprite parent "img/character.png"
    writeJSRef (spriteXScale sprite) (-1)
    return sprite

newBirdSprite :: CanHoldSprite a => a -> IO (Looping (Scaled NormalSprite))
newBirdSprite parent = newLooping parent birdImageWidth 11 5
                     $ newScaled birdImageWidth birdImageHeight birdScale
                     $ newSprite parent "img/flying-enemy.png"

newParallax :: CanHoldSprite a => a -> JSString -> IO NormalSprite
newParallax parent image = do
   parallax <- newSprite parent image
   return parallax 

-- TODO: use a random balloon image instead
newBalloonSprite :: CanHoldSprite a => a -> IO NormalSprite
newBalloonSprite parent = do
    balloon <- newEmptySprite parent
    writeJSRef (spriteSize balloon) (20, 20)
    
    dom <- getDom balloon
    set dom [style "border" =: "2px solid #880000"]
    set dom [style "border-radius" =: "10px"]
    set dom [style "background-color" =: "red"]
    
    return balloon
    
    
main :: IO ()
main = do
    scene <- newScene game_width game_height True
    loadImages scene ["img/character.png", "img/flying-enemy.png"] $ do
      back2 <- newLayer scene "back-2"
      back <- newLayer scene "back-1"
      front <- newLayer scene "front"

--      city <- newSprite front "img/city-zoomed-out.png"
--      writeJSRef (spriteSize city) (640,797)
--      writeJSRef (spritePosition city) (0,123)
--      rawUpdateSprite city
      mountain <- newSprite back2 "img/mountain-shadows.png"
      writeJSRef (spriteSize mountain) (640,920)
      writeJSRef (spritePosition mountain) (0,0)
      rawUpdateSprite mountain


      building <- newSprite back "img/city-zoomed-in.png"
      writeJSRef (spriteSize building) (640,2856)
      writeJSRef (spritePosition building) (0, 920 - 2856)
      rawUpdateSprite building

      building_shadow <- newSprite back2 "img/city-shadow.png"
      writeJSRef (spriteSize building_shadow) (640,920)
      writeJSRef (spritePosition building_shadow) (0,0)
      rawUpdateSprite building_shadow

      
      bird <- newBirdSprite front
      
      score <- newEmptySprite front
      writeJSRef (spriteSize score) (200, 100)
      writeJSRef (spritePosition score) (20, 20)
      rawUpdateSprite score
      
      dom <- getDom score
      set dom [attr "id" =: "score"]
      
      player <- newPlayerSprite front
      writeJSRef (spritePosition player) (40, 200)
      
      game_state_ref <- newIORef $ GameState
        { playerStatus     = Floating 1
        , playerSprite     = player
        , gameHeight       = 0
        , bestGameHeight   = 0
        , futureEntities   = []
        , onScreenEntities = []
        , missedEntities   = []
        }
      
      player_xv_ref <- newIORef 2.5
      score_count_ref <- newIORef 0.0
      
      ticker <- newTicker scene fps $ \ticker -> do
        ticks <- getCurrentTick ticker
        let t = computeSeconds ticks
        
        let (x, isGoingLeft) = birdState (OffScreenBird 0 0) t
        writeJSRef (spritePosition bird) (x, 100)
        writeJSRef (spriteXScale bird) (if isGoingLeft then 1 else -1)
        updateSprite bird (ticker, ())
        
        -- Update first parallax layer
        writeJSRef (spriteYPosition building) (linear (920 - 2856) 56 t)
        updateSprite building ()

        -- debugging to estimate cues
        print t

        writeJSRef (spriteYPosition building_shadow) (delayed 10 (linear 0 18) t)
        updateSprite building_shadow ()

        writeJSRef (spriteYPosition mountain) (delayed 10 (linear 0 9) t)
        updateSprite mountain ()
        
        player_xv <- readIORef player_xv_ref
        score_count <- readIORef score_count_ref
        
        modifyJSRef (spriteYVelocity player) (+ gravity)
        applyVelocity player
        
        updateSprite player (ticker, ())

        dom <- getDom score
        score_count <- readIORef score_count_ref
        set dom ["innerHTML" =: printf "Score %d" (round score_count :: Int)]
        
        y <- readJSRef (spriteYPosition player)
        when (y > game_height) $ do
          pauseTicker ticker
          alert "Game over"
        
        modifyIORef player_xv_ref (+ 0.002)
        modifyIORef score_count_ref (+ 0.08)
      runTicker ticker
