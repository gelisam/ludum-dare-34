{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.DOM
import Haste.Foreign


console_log :: JSString -> IO ()
console_log = ffi "(function(x) {console.log(x)})"

main :: IO ()
main = do
  putStrLn "it begins!"
  div <- newElem "ol" `with` [ attr "id"        =: "exec_list"
                             , style "position" =: "absolute"
                             , style "left"     =: "400px"
                             ]
  appendChild documentBody div
  ffi "visual_guide();" :: IO ()
  putStrLn "done."
