{-# LANGUAGE OverloadedStrings #-}
import Data.IORef
import Haste.Prim

import Constants
import GameState
import GameEntity
import Globals
import SpriteJS


main :: IO ()
main = do
    globals <- newGlobals
    let scene = globalScene globals
    loadImages scene [ "img/balloons.png"
                     , "img/city-shadow.png"
                     , "img/city-zoomed-in.png"
                     , "img/city-zoomed-out.png"
                     , "img/cloud-l.png"
                     , "img/cloud-m.png"
                     , "img/cloud-s.png"
                     , "img/down.png"
                     , "img/flying-enemy.png"
                     , "img/game-over.png"
                     , "img/mountain-shadows.png"
                     , "img/sky.jpg"
                     , "img/stars.png"
                     , "img/the-end.png"
                     , "img/title.png"
                     , "img/up.png"
                     , "img/wind.png"
                     ] $ do
      initialGameState <- newGameState globals
      gameStateRef <- newIORef initialGameState
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
