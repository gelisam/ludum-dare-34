{-# LANGUAGE OverloadedStrings #-}
import Data.IORef
import Haste.Prim

import Constants
import GameState
import SpriteJS


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
    let h = screenYPosition gameState
    let a = playerAge gameState
    
    gameState' <- nextGameState gameState
    drawGameState t h a ticker gameState'
    
    writeIORef gameStateRef gameState'
