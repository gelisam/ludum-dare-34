{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
module GameState where

import Control.Monad
import Control.Arrow
import Haste.DOM
import Haste.Prim
import Data.Either

import Animated
import Animation
import Backgrounds
import Collidable
import Constants
import ContentGenerator
import Centered
import Entities
import JSRef
import Looping
import Scaled
import SpriteJS
import Wrapped
import Random


data PlayerStatus
  = Falling
  | Floating Int -- number of balloons
  deriving (Show, Eq)

data PlayerDirection
  = Straight
  | West
  | East
  deriving (Show, Eq)

type PlayerSprite = Wrapped (Collidable (Looping (Scaled (Centered NormalSprite))))

data GameState = GameState
  { gameScene      :: Ptr Scene
  , gameBackLayer1 :: Ptr Layer
  , gameBackLayer2 :: Ptr Layer
  , gameFrontLayer :: Ptr Layer
  
  , playerStatus   :: PlayerStatus
  , playerDirection:: PlayerDirection
  , playerSprite   :: PlayerSprite
  , playerBalloons :: [BalloonSprite]
  , playerAge      :: Double
  
  , playerYPosition :: Double
  , playerYVelocity :: Double
  , screenYPosition :: Double
  
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


newPlayerSprite :: CanHoldSprite a => a -> IO PlayerSprite
newPlayerSprite parent = do
    sprite <- newWrapped game_width
            $ newCollidable parent (13 - 5 - playerImageWidth `div` 2) (67 - 5 - playerImageHeight `div` 2) 10 10
            $ newLooping parent playerImageWidth 1 5
            $ newScaled 1.0
            $ newCentered playerImageWidth playerImageHeight
            $ newSprite parent "img/up.png"
    return sprite

newBirdSprite :: CanHoldSprite a
              => a
              -> OffScreenBird
              -> IO (BirdSprite)
newBirdSprite parent offScreenBird = newDirectionalMoving (birdAnimation offScreenBird)
                                   $ newCollidable parent (-32) (-32) 64 64
                                   $ newLooping parent birdImageWidth 11 5
                                   $ newScaled birdScale
                                   $ newCentered birdImageWidth birdImageHeight
                                   $ newSprite parent "img/flying-enemy.png"

-- TODO: use a random balloon image instead
newBalloonSprite :: CanHoldSprite a => a -> IO BalloonSprite
newBalloonSprite parent = do
    balloon <- newWrapped game_width
             $ newCollidable parent (-30) (-70) 50 50
             $ newScaled balloonScale
             $ newCentered balloonImageWidth balloonImageHeight
             $ newSprite parent "img/balloons.png"
    r <- randomRIO (0,3)
    let offset = 475 * (r :: Int)
    writeJSRef (spriteYOffset balloon) (fromIntegral offset)
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
    
    offScreenBirds <- generateBirds
    onScreenBirds <- forM offScreenBirds $ \offScreenBird -> do
      birdSprite <- newBirdSprite front offScreenBird
      return $ OnScreenBird birdSprite offScreenBird
    
    playerSprite <- newPlayerSprite front
    writeJSRef (spritePosition playerSprite) (playerInitialXPosition, playerInitialYPosition)

    input <- newInput scene

    initialBalloon <- newBalloonSprite front
    positionBalloon playerSprite initialBalloon Straight
    
    return $ GameState
      { gameScene      = scene
      , gameBackLayer1 = back1
      , gameBackLayer2 = back2
      , gameFrontLayer = front
      
      , playerStatus     = Floating 1
      , playerDirection  = Straight
      , playerSprite     = playerSprite
      , playerBalloons   = [initialBalloon]
      , playerAge        = 0
      
      , playerYPosition = playerInitialYPosition
      , playerYVelocity = 0
      , screenYPosition = 0
  
      , scoreSprite = scoreSprite
      , gameScore   = 0
      
      , entitiesBelow   = []
      , currentEntities = map BirdOn onScreenBirds
      , entitiesAbove   = []
      
      , backgroundsBelow   = []
      , currentBackgrounds = [ ParallaxOn onScreenMountain
                             , ParallaxOn onScreenBuilding
                             , ParallaxOn onScreenBuildingShadow
                             ]
      , backgroundsAbove   = []
      , input = input
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

nextXVelocity :: SpriteLike a => Double -> Double -> a -> IO ()
nextXVelocity maxV amount sprite = do
    vx <- readJSRef (spriteXVelocity sprite)
    let vx' = if maxV < 0 then max (vx + amount) maxV else min (vx + amount) maxV
    writeJSRef (spriteXVelocity sprite) vx'
    applyVelocity sprite

nextPlayerVelocity :: SpriteLike a => PlayerDirection -> a -> IO () 
nextPlayerVelocity Straight player = (nextXVelocity (-5) (-0.05) player)
nextPlayerVelocity West     player = (nextXVelocity (-9.6) (-0.8) player)
nextPlayerVelocity East     player = (nextXVelocity  9.6  0.8 player)

positionBalloon :: SpriteLike a => SpriteLike b => a -> b -> PlayerDirection -> IO ()
positionBalloon player balloon dir = do
    (px, py) <- readJSRef (spritePosition player)
    let bx = px - case dir of
                    Straight -> balloonWidth/1.5
                    West     -> balloonWidth/1.2
                    East     -> balloonWidth/2
    let by = py - balloonHeight
    writeJSRef (spritePosition balloon) (bx, by)

nextGameState :: GameState -> IO GameState
nextGameState (g@GameState {..}) = do
    going_left <- leftdown input
    going_right <- rightdown input
    let playerDirection' = if going_left then West else if going_right then East else Straight
    nextPlayerVelocity playerDirection' playerSprite
  
    -- birds, balloons
    let (birds, _) = partitionEithers $ flip map currentEntities $ \case
                 BirdOn bird -> Left (birdSprite bird)
                 BalloonOn balloon -> Right (balloonSprite balloon)

    balloonCollisions <- flip mapM playerBalloons $ \balloon -> do
          collides <- collidesWithList balloon birds
          return $ if collides then Left balloon else Right balloon
      
    let (pop, keep) = partitionEithers balloonCollisions

    let playerStatus' = if null keep
                        then Falling
                        else Floating (length keep)
        playerYPosition' = playerYPosition + playerYVelocity
        playerYVelocity' = case playerStatus' of
          Falling    -> playerYVelocity - 0.5
          Floating n -> 56 / 25 + fromIntegral (n-1) * (5.6 / 25)
        screenYPosition' = playerYPosition' - playerInitialYPosition

    mapM_ removeSprite pop
    mapM_ (\b -> positionBalloon playerSprite b playerDirection') keep 

    writeJSRef (spriteYPosition playerSprite) (playerYPosition' - screenYPosition')
    
    when (playerStatus /= Falling) $ do
      writeJSRef (spriteXOffset playerSprite) $ case playerDirection' of
        Straight -> 0
        West     -> playerImageWidth
        East     -> playerImageWidth * 2
    
    return $ g
      { playerStatus    = playerStatus'
      , playerDirection = playerDirection'
      , playerBalloons  = keep
      
      , playerYPosition = playerYPosition'
      , playerYVelocity = playerYVelocity'
      , screenYPosition = screenYPosition'
      }

drawGameState :: Double -> Double -> Double -> Ptr Ticker -> GameState -> IO ()
drawGameState t h a ticker (GameState {..}) = do
    updateSprite playerSprite (ticker, ())
    mapM_ (drawOnScreenEntity     t h a ticker) currentEntities
    mapM_ (drawOnScreenBackground t h a ticker) currentBackgrounds
    forM_ playerBalloons $ \balloon -> updateSprite balloon ()
    
    -- throws an exception for some reason?
    --dom <- getDom scoreSprite
    --set dom ["innerHTML" =: printf "Score %d" (round gameScore :: Int)]
