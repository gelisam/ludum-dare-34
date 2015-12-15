{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.IORef
import Haste.Prim

import Centered
import Constants
import GameState
import GameEntity
import Globals
import JSRef
import Scaled
import SpriteJS


main :: IO ()
main = do
    scene <- newScene game_width game_height True
    titleScreen scene

titleScreen :: Ptr Scene -> IO ()
titleScreen scene = do
    resetScene scene
    input <- newInput scene
    loadImages scene [ "img/city-zoomed-out.png"
                     , "img/title.png"
                     ] $ do
      
      background <- newTopLeftAligned 640 797
                  $ newSprite scene "img/city-zoomed-out.png"
      writeJSRef (spriteYPosition background) (game_height - 797)
      
      title <- newCentered 640 673
             $ newScaled 0.9
             $ newSprite scene "img/title.png"
      writeJSRef (spriteYPosition title) 330
      
      updateSprite background ()
      updateSprite title      ()
      
      ticker <- newTicker scene 10 $ \ticker -> do
          r <- keydown input
          when r $ do
            mainGame scene
      runTicker ticker


mainGame :: Ptr Scene -> IO ()
mainGame scene = do
    resetScene scene
    globals <- newGlobals scene
    let scene = globalScene globals
    loadImages scene [ "img/balloons.png"
                     , "img/city-shadow.png"
                     , "img/city-zoomed-in.png"
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
