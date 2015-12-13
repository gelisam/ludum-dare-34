{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module GameState where

import Control.Arrow
import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Prim
import Haste.Foreign
import Text.Printf

import Animated
import Animation
import Backgrounds
import Constants
import Centered
import Entities
import JSRef
import Looping
import Random
import Scaled
import SpriteJS
import WindowJS


data PlayerStatus
  = Falling
  | Floating Int -- number of balloons
  deriving (Show, Eq)


data GameState = GameState
  { gameScene      :: Ptr Scene
  , gameBackLayer1 :: Ptr Layer
  , gameBackLayer2 :: Ptr Layer
  , gameFrontLayer :: Ptr Layer
  
  , playerStatus   :: PlayerStatus
  , playerSprite   :: Looping (Scaled (Centered NormalSprite))
  , gameHeight     :: Double
  , bestGameHeight :: Double
  
  , scoreSprite :: NormalSprite
  , gameScore   :: Double
  
  , entitiesBelow   :: [OffScreenEntity]
  , currentEntities :: [OnScreenEntity]
  , entitiesAbove   :: [OffScreenEntity]
  
  , backgroundsBelow   :: [OffScreenBackground]
  , currentBackgrounds :: [OnScreenBackground]
  , backgroundsAbove   :: [OffScreenBackground]

  , input :: Ptr Input
  }


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


newGameState :: IO GameState
newGameState = do
    scene <- newScene game_width game_height True
    back2 <- newLayer scene "back-2"
    back1 <- newLayer scene "back-1"
    front <- newLayer scene "front"
    
    
    scoreSprite <- newTopLeftAligned 200 100
           $ newEmptySprite front
    writeJSRef (spritePosition scoreSprite) (20, 20)
    
    dom <- getDom scoreSprite
    set dom [attr "id" =: "score"]
    
    
    let mountainImage = "img/mountain-shadows.png"
    let mountainAnimation = delayed 10
                          $ linear 0 9
    let offScreenMountain = OffScreenParallaxLayer mountainImage mountainAnimation
    mountainSprite <- newParallax mountainAnimation
                    $ newTopLeftAligned 640 920
                    $ newSprite back2 mountainImage
    let onScreenMountain = OnScreenParallaxLayer mountainSprite offScreenMountain
    
    let buildingImage = "img/city-zoomed-in.png"
    let buildingAnimation = linear (920 - 2856) 56
    let offScreenBuilding = OffScreenParallaxLayer buildingImage buildingAnimation
    buildingSprite <- newParallax buildingAnimation
                    $ newTopLeftAligned 640 2856
                    $ newSprite back1 "img/city-zoomed-in.png"
    let onScreenBuilding = OnScreenParallaxLayer buildingSprite offScreenBuilding
    
    let buildingShadowImage = "img/city-shadow.png"
    let buildingShadowAnimation = delayed 10
                                $ linear 0 18
    let offScreenBuildingShadow = OffScreenParallaxLayer buildingShadowImage buildingShadowAnimation
    buildingShadowSprite <- newParallax buildingShadowAnimation
                          $ newTopLeftAligned 640 920
                          $ newSprite back2 buildingShadowImage
    let onScreenBuildingShadow = OnScreenParallaxLayer buildingShadowSprite offScreenBuildingShadow
    
    let offScreenBird = (OffScreenBird 0 0)
    birdSprite <- newBirdSprite front offScreenBird
    let onScreenBird = OnScreenBird birdSprite offScreenBird
    
    playerSprite <- newPlayerSprite front
    writeJSRef (spritePosition playerSprite) (40, 200)
    
    return $ GameState
      { gameScene      = scene
      , gameBackLayer1 = back1
      , gameBackLayer2 = back2
      , gameFrontLayer = front
      
      , playerStatus     = Floating 1
      , playerSprite     = playerSprite
      , gameHeight       = 0
      , bestGameHeight   = 0
      
      , entitiesBelow   = []
      , currentEntities = [ BirdOn onScreenBird
                          ]
      , entitiesAbove   = []
      
      , backgroundsBelow   = []
      , currentBackgrounds = [ ParallaxOn onScreenMountain
                             , ParallaxOn onScreenBuilding
                             , ParallaxOn onScreenBuildingShadow
                             ]
      , backgroundsAbove   = []
      }

-- move game objects between the on-screen and off-screen buffers.
-- 
-- For Ordering, GT means entirely above the screen, EQ means at least partially on screen,
-- and LT means entirely below the screen.
-- 
-- Game objects in the first buffer (below the screen) should be order from top to bottom,
-- game objects in the second buffer (on the screen) should be ordered from bottom to top,
-- game objects in the last buffer (above the screen) should be ordered from bottom to top.
shuffleZipper :: Monad m
              => (h -> offScreen -> Ordering)
              -> (h -> onScreen  -> Ordering)
              -> (offScreen -> m onScreen)
              -> (onScreen -> m offScreen)
              -> h
              -> ([offScreen], [onScreen], [offScreen])
              -> m ([offScreen], [onScreen], [offScreen])
shuffleZipper isOffVisible isOnVisible putOnScreen takeOffScreen h (below, current, above) = do
    newlyBelow' <- mapM takeOffScreen newlyBelow
    newlyAbove' <- mapM takeOffScreen newlyAbove
    belowCurrent' <- mapM putOnScreen belowCurrent
    aboveCurrent' <- mapM putOnScreen aboveCurrent
    
    let below'   = reverse newlyBelow' ++ stillBelow
    let current' = reverse belowCurrent' ++ stillCurrent ++ aboveCurrent'
    let above'   = newlyAbove' ++ stillAbove
    
    return (below', current', above')
  where
    (belowCurrent, stillBelow) = span (isOffVisible h >>> (/= LT)) below
    (aboveCurrent, stillAbove) = span (isOffVisible h >>> (/= GT)) above
    
    newlyBelow   = filter (isOnVisible h >>> (== LT)) current
    stillCurrent = filter (isOnVisible h >>> (== EQ)) current
    newlyAbove   = filter (isOnVisible h >>> (== GT)) current

nextGameState :: Double -> Double -> Double -> Ptr Ticker -> GameState -> IO GameState
nextGameState t h a ticker (GameState {..}) = do
    return $ GameState
      { playerStatus   = playerStatus'
      , playerSprite   = playerSprite'
      , gameHeight     = gameHeight'
      , bestGameHeight = bestGameHeight'
      
      , entitiesBelow   = entitiesBelow'
      , currentEntities = currentEntities'
      , entitiesAbove   = entitiesAbove'
      
      , backgroundsBelow   = backgroundsBelow'
      , currentBackgrounds = currentBackgrounds'
      , backgroundsAbove   = backgroundsAbove'
      }
  where
    playerStatus'   = playerStatus
    playerSprite'   = playerSprite
    gameHeight'     = gameHeight
    bestGameHeight' = bestGameHeight
    
    entitiesBelow'   = entitiesBelow
    currentEntities' = currentEntities
    entitiesAbove'   = entitiesAbove
    
    backgroundsBelow'   = backgroundsBelow
    currentBackgrounds' = currentBackgrounds
    backgroundsAbove'   = backgroundsAbove
    

drawGameState :: Double -> Double -> Double -> Ptr Ticker -> GameState -> IO ()
drawGameState t h a ticker (GameState {..}) = do
    updateSprite playerSprite (ticker, ())
    mapM_ (drawOnScreenEntity     t h a ticker) currentEntities
    mapM_ (drawOnScreenBackground t h a ticker) currentBackgrounds
    
    -- throws an exception for some reason?
    --dom <- getDom scoreSprite
    --set dom ["innerHTML" =: printf "Score %d" (round gameScore :: Int)]
