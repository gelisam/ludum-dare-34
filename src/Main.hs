{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow
import Control.Monad
import Data.IORef
import Haste
import Haste.Foreign
import Haste.Prim
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


main :: IO ()
main = do
    initialGameState <- newGameState
    gameStateRef <- newIORef initialGameState
    let scene = gameScene initialGameState
    loadImages scene ["img/up.png", "img/flying-enemy.png"] $ do
      ticker <- newTicker scene fps (gameLoop gameStateRef)
      runTicker ticker

gameLoop :: IORef GameState -> Ptr Ticker -> IO ()
gameLoop gameStateRef ticker = do
    gameState <- readIORef gameStateRef
    
    ticks <- getCurrentTick ticker
    let t = computeSeconds ticks
    let h = 56 * t  -- for now, the screen rises 1 unit per second. Wait, is that 1 pixel?
    let a = h  -- for now, the character ages at the same speed as the screen rises.
    
    gameState' <- nextGameState t h a ticker gameState
    drawGameState t h a ticker gameState'
    
    writeIORef gameStateRef gameState'
    ticks `seq` t `seq` h `seq` a `seq` return ()
