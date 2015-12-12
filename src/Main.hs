{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Foreign
import Haste.Prim
import Text.Printf

import JSRef
import Random
import SpriteJS
import WindowJS


main :: IO ()
main = do
    putStrLn "it begins!"
    
    let game_width = 640
    let game_height = 920
    scene <- newScene game_width game_height True
    loadImages scene ["img/character.png", "img/crate.png"] $ do
      back <- newLayer scene "backround"
      front <- newLayer scene "front"
      
      score <- newEmptySprite front
      setSpriteSize score 200 100
      setSpritePosition score 20 20
      updateSprite score
      
      dom <- getDom score
      set dom [attr "id" =: "score"]
      
      bottom <- newSprite back "img/crate.png"
      setSpriteSize bottom game_width 64
      setSpritePosition bottom 0 (game_height-64)
      updateSprite bottom
      
      elements <- newSpriteList
      appendToSpriteList elements bottom
      
      player <- newSprite front "img/character.png"
      setSpritePosition player 40 200
      setSpriteSize player 28 52
      setSpriteXYScale player (-1) 1
      
      input <- newInput scene
      
      cycle <- newCycle scene [ (3, 3, 5)
                              , (33, 3, 5)
                              , (63, 3, 5)
                              , (93, 3, 5)
                              , (123, 3, 5)
                              , (153, 3, 5)
                              , (183, 3, 5)
                              ]
      appendToCycle cycle player
      
      player_xv_ref <- newIORef 2.5
      score_count_ref <- newIORef 0.0
      
      ticker <- newTicker scene 25 $ \ticker -> do
        player_xv <- readIORef player_xv_ref
        score_count <- readIORef score_count_ref
        
        let gravity = 0.5
        modifyJSRef (yVelocity player) (+ gravity)
        applyXVelocity player
        
        r <- collidesWithSpriteList player elements
        case r of
          Just otherSprite -> do
            pauseTicker ticker
            alert "Game over!"
          Nothing -> do
            applyYVelocity player
            
            r <- collidesWithSpriteList player elements
            case r of
              Just otherSprite -> do
                unapplyYVelocity player
                writeJSRef (yVelocity player) 0
                
                r <- (||) <$> mousedown input <*> keydown input
                when r $ do
                  modifyJSRef (yVelocity player) (subtract 10)
              Nothing -> do
                return ()
                r <- (||) <$> mousedown input <*> keydown input
                when r $ do
                  modifyJSRef (yVelocity player) (subtract 0.2)
            updateSprite player
            
            el_ref <- newIORef (undefined :: Ptr Sprite)
            need_to_create_plateform_ref <- newIORef True
            
            forEachSprite elements $ \el -> do
              player_xv <- readIORef player_xv_ref
              writeJSRef (xVelocity el) (-player_xv)
              
              applyVelocity el
              updateSprite el
              
              r <- isPointIn el game_width (game_height - 20)
              when r $ do
                writeIORef need_to_create_plateform_ref False
              
              x <- getSpriteX el
              w <- getSpriteWidth el
              when (x + w < 0) $ do
                removeFromSpriteList elements el
            
            r <- readIORef need_to_create_plateform_ref
            rand <- randomRIO (0.0, 1.0::Double)
            when (r && rand < 0.1) $ do
              height <- randomRIO (32, 32 + 96)
              width  <- randomRIO (64, 64 + 128)
              bottom <- newSprite back "img/crate.png"
              setSpriteSize bottom width height
              setSpritePosition bottom game_width (game_height-height)
              updateSprite bottom
              appendToSpriteList elements bottom
            
            updateCycle cycle ticker
    
            dom <- getDom score
            score_count <- readIORef score_count_ref
            set dom ["innerHTML" =: printf "Score %d" (round score_count :: Int)]
            
            y <- getSpriteY player
            when (y > game_height) $ do
              pauseTicker ticker
              alert "Game over"
            
            modifyIORef player_xv_ref (+ 0.002)
            modifyIORef score_count_ref (+ 0.08)
      runTicker ticker
    
    putStrLn "done."
