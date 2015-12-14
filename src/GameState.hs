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
import Balloon
import Bird
import Collidable
import Constants
import ContentGenerator
import Centered
import Data.List.Extra
import Entities
import Globals
import HeldBalloon
import JSRef
import Looping
import Scaled
import SpriteJS
import Wrapped


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
  { gameGlobals :: Globals
  
  , playerStatus   :: PlayerStatus
  , playerDirection:: PlayerDirection
  , playerSprite   :: (PlayerSprite, PlayerSprite)
  , playerBalloons :: [HeldBalloonSprite]
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

newPlayerSprite :: CanHoldSprite a => a -> IO PlayerSprite
newPlayerSprite parent = do
    sprite <- newWrapped game_width
            $ newCollidable parent (13 - 5 - playerImageWidth `div` 2) (67 - 5 - playerImageHeight `div` 2) 10 10
            $ newLooping parent playerImageWidth 1 5
            $ newScaled 1.0
            $ newCentered playerImageWidth playerImageHeight
            $ newSprite parent "img/up.png"
    return sprite

newFallingPlayerSprite :: CanHoldSprite a => a -> IO PlayerSprite
newFallingPlayerSprite parent = do
    sprite <- newWrapped game_width
            $ newCollidable parent (13 - 5 - playerImageWidth `div` 2) (67 - 5 - playerImageHeight `div` 2) 10 10
            $ newLooping parent playerImageWidth 4 5
            $ newScaled 1.0
            $ newCentered playerImageWidth playerImageHeight
            $ newSprite parent "img/down.png"
    return sprite


newGameState :: Globals -> IO GameState
newGameState (globals@Globals {..}) = do
    scoreSprite <- newTopLeftAligned 200 100
                 $ newEmptySprite globalFrontLayer
    writeJSRef (spritePosition scoreSprite) (20, 20)
    
    dom <- getDom scoreSprite
    set dom [attr "id" =: "score"]
    
    
    let mountainImage = "img/mountain-shadows.png"
    let mountainAnimation = delayed 10
                          $ linear 0 9
    let offScreenMountain = OffScreenParallaxLayer mountainImage mountainAnimation
    mountainSprite <- newParallax mountainAnimation
                    $ newTopLeftAligned 640 920
                    $ newSprite globalBackLayer2 mountainImage
    let onScreenMountain = OnScreenParallaxLayer mountainSprite offScreenMountain
    
    let buildingImage = "img/city-zoomed-in.png"
    let buildingAnimation = delayed 0
                          $ linear (920 - 2856) 56
    let offScreenBuilding = OffScreenParallaxLayer buildingImage buildingAnimation
    buildingSprite <- newParallax buildingAnimation
                    $ newTopLeftAligned 640 2856
                    $ newSprite globalBackLayer1 "img/city-zoomed-in.png"
    let onScreenBuilding = OnScreenParallaxLayer buildingSprite offScreenBuilding
    
    let buildingShadowImage = "img/city-shadow.png"
    let buildingShadowAnimation = delayed 10
                                $ linear 0 18
    let offScreenBuildingShadow = OffScreenParallaxLayer buildingShadowImage buildingShadowAnimation
    buildingShadowSprite <- newParallax buildingShadowAnimation
                          $ newTopLeftAligned 640 920
                          $ newSprite globalBackLayer2 buildingShadowImage
    let onScreenBuildingShadow = OnScreenParallaxLayer buildingShadowSprite offScreenBuildingShadow

    let gameOverAnimation = delayed (-20)
                          $ linear 0 50
    let offScreenGameOver = OffScreenParallaxLayer "img/game-over.png" gameOverAnimation
    gameOverSprite <- newParallax gameOverAnimation
                    $ newTopLeftAligned 640 673
                    $ newSprite globalBackLayer1 "img/game-over.png"
    let onScreenGameOver = OnScreenParallaxLayer gameOverSprite offScreenGameOver
    
    offScreenBalloons <- generateBalloons
    offScreenBirds <- generateBirds
    let futureEntities = mergeOn entityYPosition (map BalloonOff offScreenBalloons)
                                                 (map BirdOff    offScreenBirds   )
    
    playerSprite <- newPlayerSprite globalFrontLayer
    writeJSRef (spritePosition playerSprite) (playerInitialXPosition, playerInitialYPosition)

    fallingSprite <- newFallingPlayerSprite globalFrontLayer
    writeJSRef (spritePosition fallingSprite) (playerInitialXPosition, playerInitialYPosition)

    input <- newInput globalScene

    initialBalloon <- newHeldBalloonSprite globalFrontLayer 3
    positionBalloon playerSprite initialBalloon Straight
    
    return $ GameState
      { gameGlobals = globals
      
      , playerStatus     = Floating 1
      , playerDirection  = Straight
      , playerSprite     = (playerSprite, fallingSprite)
      , playerBalloons   = [initialBalloon]
      , playerAge        = 0

      
      , playerYPosition = playerInitialYPosition
      , playerYVelocity = 0
      , screenYPosition = playerInitialYPosition - playerInitialYPosition
  
      , scoreSprite = scoreSprite
      , gameScore   = 0
      
      , entitiesBelow   = []
      , currentEntities = []
      , entitiesAbove   = futureEntities
      
      , backgroundsBelow   = []
      , currentBackgrounds = [ ParallaxOn onScreenMountain
                             , ParallaxOn onScreenBuilding
                             , ParallaxOn onScreenBuildingShadow
                             , ParallaxOn onScreenGameOver
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
    nextPlayerVelocity playerDirection' $ fst playerSprite
    nextPlayerVelocity playerDirection' $ snd playerSprite

    let indexedEntities = zip [0::Int ..] currentEntities
    -- birds, balloons
    let (birds, balloons) = partitionEithers $ flip map indexedEntities $ \case
                 (key, BirdOn bird) -> Left (key, birdSprite bird)
                 (key, BalloonOn balloon) -> Right (key, balloonSprite balloon)

    popOrKeep <- flip mapM playerBalloons $ \heldBalloon -> do
          collides <- collidesWithList heldBalloon (map snd birds)
          return $ if collides then Left heldBalloon else Right heldBalloon

    let (pop, keep) = partitionEithers popOrKeep

    mapM_ removeSprite pop
    mapM_ (\b -> positionBalloon (fst playerSprite) b playerDirection') keep 

    grabOrDrop <- flip mapM balloons $ \(key, balloon) -> do
          collides <- collidesWith balloon (fst playerSprite)
          return $ if collides then Left (key, balloon) else Right (key, balloon)

    let (grab, _) = partitionEithers grabOrDrop

    let grabbedIndices = map fst grab
    let grabbedSprites = map snd grab
    newHeldBalloons <- forM grabbedSprites $ \s -> do
          (_, offset) <- readJSRef (spriteOffset s)
          makeHeldBalloonSprite (globalFrontLayer gameGlobals) offset

    let playerBalloons' = keep ++ newHeldBalloons
    mapM_ removeSprite grabbedSprites

    let remaining = map snd $ filter (\(k,_) -> k `notElem` grabbedIndices) indexedEntities

    let playerStatus' = if null playerBalloons'
                        then Falling
                        else Floating (length playerBalloons')
        playerYPosition' = playerYPosition + playerYVelocity
        playerYVelocity' = case playerStatus' of
          Falling    -> max (-50) (playerYVelocity - 0.5)
          Floating n -> let target = 56 / 25 + fromIntegral (n-1) * (5.6 / 25)
                         in min target (playerYVelocity + 0.5)
        screenYTarget = case playerStatus' of
          Falling    -> playerYPosition' - 200
          Floating _ -> playerYPosition' - playerInitialYPosition
        screenYPosition' = screenYPosition + playerYVelocity'
        screenYPosition'' = screenYPosition' + (screenYTarget - screenYPosition') * 0.1
        screenYNegative = -screenYPosition''  -- hack because I messed up the Y direction

    writeJSRef (spriteYPosition (fst playerSprite)) (playerYPosition' - screenYPosition')
    writeJSRef (spriteYPosition (snd playerSprite)) (playerYPosition' - screenYPosition')
   
    when (playerStatus /= Falling) $ do
      writeJSRef (spriteXOffset (fst playerSprite)) $ case playerDirection' of
        Straight -> 0
        West     -> playerImageWidth
        East     -> playerImageWidth * 2
    
    (entitiesBelow', currentEntities', entitiesAbove') <-
      shuffleZipper isEntityVisible                 isEntityStillVisible
                    (putEntityOnScreen gameGlobals) takeEntityOffScreen
                    screenYNegative
                    (entitiesBelow, remaining, entitiesAbove)
    
    if (playerStatus /= Falling) then do
      writeJSRef (spriteOpacity (fst playerSprite)) 100
      writeJSRef (spriteOpacity (snd playerSprite)) 0
    else do
      writeJSRef (spriteOpacity (fst playerSprite)) 0
      writeJSRef (spriteOpacity (snd playerSprite)) 100

    let playerAge' = max playerAge screenYPosition
    -- age every 1400 pixels
--   let ageOffset  = floor (playerAge / 1400)
    let ageOffset' = floor (playerAge' / 1400)

--    
--    Doing this only on age changes flickers the character and brings it back
--    to the youngest version, why?
--    if (ageOffset < ageOffset') then do
    writeJSRef (spriteYOffset (fst playerSprite)) $ (fromIntegral ageOffset') * 300
    writeJSRef (spriteYOffset (snd playerSprite)) $ (fromIntegral ageOffset') * 300 
--    else return ()


    return $ g
      { playerStatus    = playerStatus'
      , playerDirection = playerDirection'
      , playerBalloons  = playerBalloons'
      , playerAge       = playerAge'
      
      , playerYPosition = playerYPosition'
      , playerYVelocity = playerYVelocity'
      , screenYPosition = screenYPosition''
      
      , entitiesBelow   = entitiesBelow'
      , currentEntities = currentEntities'
      , entitiesAbove   = entitiesAbove'
      }

drawGameState :: Double -> Double -> Double -> Ptr Ticker -> GameState -> IO ()
drawGameState t h a ticker (GameState {..}) = do
    updateSprite (fst playerSprite) (ticker, ())
    updateSprite (snd playerSprite) (ticker, ())
    mapM_ (drawOnScreenEntity     t h a ticker) currentEntities
    mapM_ (drawOnScreenBackground t h a ticker) currentBackgrounds
    forM_ playerBalloons $ \balloon -> updateSprite balloon ()
    
    -- throws an exception for some reason?
    --dom <- getDom scoreSprite
    --set dom ["innerHTML" =: printf "Score %d" (round gameScore :: Int)]
