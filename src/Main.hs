{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow
import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Foreign
import Text.Printf

import Animated
import Animation
import Constants
import Centered
import Entities
import GameState
import JSRef
import Looping
import Random
import Scaled
import SpriteJS
import WindowJS


computeSeconds :: Int -> Double
computeSeconds ticks = fromIntegral ticks / fps

birdAnimation :: OffScreenBird -> Animation ((Double, Double), Bool)
birdAnimation offScreenBird = fmap go xAndIsGoingLeft
  where
    xAndIsGoingLeft :: Animation (Double, Bool)
    xAndIsGoingLeft = bounce (birdWidth / 2, game_width - birdWidth / 2)
                    $ flip linear birdPixelsPerSecond
                    $ birdInitialX offScreenBird
    
    go :: (Double,Bool) -> ((Double, Double), Bool)
    go (x,isGoingLeft) = ((x,y),isFlipped)
      where
        y = birdInitialY offScreenBird
        isFlipped = not isGoingLeft


newPlayerSprite :: CanHoldSprite a => a -> IO (Looping (Scaled (Centered NormalSprite)))
newPlayerSprite parent = do
    sprite <- newLooping parent playerImageWidth 1 5
            $ newScaled 1.0
            $ newCentered playerImageWidth playerImageHeight
            $ newSprite parent "img/up.png"
    writeJSRef (spriteXScale sprite) (-1)
    return sprite

newBirdSprite :: CanHoldSprite a
              => a
              -> OffScreenBird
              -> IO (Animated (Looping (Scaled (Centered NormalSprite))))
newBirdSprite parent offScreenBird = newDirectionalMoving (birdAnimation offScreenBird)
                                   $ newLooping parent birdImageWidth 11 5
                                   $ newScaled birdScale
                                   $ newCentered birdImageWidth birdImageHeight
                                   $ newSprite parent "img/flying-enemy.png"

-- TODO: use a random balloon image instead
newBalloonSprite :: CanHoldSprite a => a -> IO NormalSprite
newBalloonSprite parent = do
    balloon <- newTopLeftAligned 20 20
             $ newEmptySprite parent
    
    dom <- getDom balloon
    set dom [style "border" =: "2px solid #880000"]
    set dom [style "border-radius" =: "10px"]
    set dom [style "background-color" =: "red"]
    
    return balloon


main :: IO ()
main = do
    scene <- newScene game_width game_height True
    loadImages scene ["img/up.png", "img/flying-enemy.png"] $ do
      back2 <- newLayer scene "back-2"
      back <- newLayer scene "back-1"
      front <- newLayer scene "front"

--      city <- newTopLeftAligned 640 797
--            $ newSprite front "img/city-zoomed-out.png"
--      writeJSRef (spritePosition city) (0,123)
--      updateSprite city ()
      mountain <- newParallax (delayed 10 $ linear 0 9)
                $ newTopLeftAligned 640 920
                $ newSprite back2 "img/mountain-shadows.png"
      building <- newParallax (linear (920 - 2856) 56)
                $ newTopLeftAligned 640 2856
                $ newSprite back "img/city-zoomed-in.png"
      building_shadow <- newParallax (delayed 10 $ linear 0 18)
                       $ newTopLeftAligned 640 920
                       $ newSprite back2 "img/city-shadow.png"

      
      bird <- newBirdSprite front (OffScreenBird 0 0)
      
      score <- newTopLeftAligned 200 100
             $ newEmptySprite front
      writeJSRef (spritePosition score) (20, 20)
      updateSprite score ()
      
      dom <- getDom score
      set dom [attr "id" =: "score"]
      
      player <- newPlayerSprite front
      writeJSRef (spritePosition player) (40, 200)
      
      game_state_ref <- newIORef $ GameState
        { playerStatus     = Floating 1
        , playerSprite     = player
        , gameHeight       = 0
        , bestGameHeight   = 0
        
        , entitiesBelow   = []
        , currentEntities = []
        , entitiesAbove   = []
        
        , backgroundsBelow   = []
        , currentBackgrounds = []
        , backgroundsAbove   = []
        }
      
      player_xv_ref <- newIORef 2.5
      score_count_ref <- newIORef 0.0
      
      ticker <- newTicker scene fps $ \ticker -> do
        ticks <- getCurrentTick ticker
        let t = computeSeconds ticks
        let h = 56 * t  -- for now, the screen rises 1 unit per second. Wait, is that 1 pixel?
        let a = h  -- for now, the character ages at the same speed as the screen rises.
        
        updateSprite bird (t, h, a, (ticker, ()))
        
        -- Update first parallax layer
        updateSprite building (t, h, a, ())

        -- debugging to estimate cues
        print t

        updateSprite building_shadow (t, h, a, ())

        updateSprite mountain (t, h, a, ())
        
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
