{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.Foreign


console_log :: JSString -> IO ()
console_log = ffi "(function(x) {console.log(x)})"

main :: IO ()
main = do
  putStrLn "typechecks."
  console_log "it talks!"
