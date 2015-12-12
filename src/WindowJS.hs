{-# LANGUAGE OverloadedStrings #-}
module WindowJS where

import Haste.Foreign
import Haste.Prim


windowWidth :: IO Int
windowWidth = ffi "(function() {return window.innerWidth;})"

windowHeight :: IO Int
windowHeight = ffi "(function() {return window.innerHeight;})"

setTimeout :: Int -> IO () -> IO ()
setTimeout = ffi "(function(millis,callback) {window.setTimeout(callback,millis);})"
