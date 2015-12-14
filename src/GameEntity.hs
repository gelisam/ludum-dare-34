{-# LANGUAGE FunctionalDependencies #-}
module GameEntity where

import Globals


class GameEntity on off | on  -> off
                        , off -> on
  where
    ofScreen      :: on -> off
    verticalSize  :: off -> (Double, Double)
    
    putOnScreen   :: Globals -> off -> IO on
    takeOffScreen :: on -> IO off
