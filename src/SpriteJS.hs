{-# LANGUAGE OverloadedStrings #-}
module SpriteJS where

import Haste.Foreign
import Haste.Prim


newtype Scene = Scene JSAny
newtype Surface = Surface JSAny
newtype Sprite = Sprite JSAny


setDebug :: Bool -> IO ()
setDebug = ffi "(function(b) {sjs.debug = b;})"


newScene :: Int -> Int -> Bool -> IO (Ptr Scene)
newScene = ffi "(function(w,h,b) {return sjs.Scene({w:w, h:h, autoPause:b});})"

loadImages :: Ptr Scene -> [JSString] -> IO () -> IO ()
loadImages = ffi "(function(scene,images,callback) {scene.loadImages(images,callback);})"

setMainCallback :: Ptr Scene -> IO () -> IO ()
setMainCallback = ffi "(function(scene,callback) {scene.main = callback;})"

loadMap :: Ptr Scene -> JSString -> IO ()
loadMap = ffi "(function(scene,json_filename) {sjs.map.loadMap(json_filename, scene);})"


newScrollingSurface :: Ptr Scene -> IO (Ptr Surface)
newScrollingSurface = ffi
   "(function(scene) {                                                                \
   \   return sjs.ScrollingSurface(scene, scene.w, scene.h, function(layer, _x, _y) { \
   \       sjs.map.paintOn(layer, _x, _y);                                            \
   \   });                                                                            \
   \})"

updateSurface :: Ptr Surface -> IO ()
updateSurface = ffi "(function(surface) {surface.update();})"


newSprite :: Ptr Scene -> JSString -> IO (Ptr Sprite)
newSprite = ffi "(function(scene,image) {return scene.Sprite(image);})"

setSpritePosition :: Ptr Sprite -> Int -> Int -> IO ()
setSpritePosition = ffi "(function(sprite,x,y) {sprite.position(x,y);})"

moveSpriteBy :: Ptr Sprite -> Int -> Int -> IO ()
moveSpriteBy = ffi "(function(sprite,dx,dy) {sprite.move(dx,dy);})"

-- in turns, not degrees nor radians
rotateSpriteBy :: Ptr Sprite -> Double -> IO ()
rotateSpriteBy = ffi "(function(sprite,turns) {sprite.rotate(2*Math.PI*turns);})"

setSpriteXYScale :: Ptr Sprite -> Double -> Double -> IO ()
setSpriteXYScale = ffi "(function(sprite,xfactor,yfactor) {sprite.scale(xfactor,yfactor);})"

-- uniform scale
setSpriteScale :: Ptr Sprite -> Double -> IO ()
setSpriteScale sprite factor = setSpriteXYScale sprite factor factor

-- 1.0 for opaque, 0.0 for invisible
setSpriteOpacity :: Ptr Sprite -> Double -> IO ()
setSpriteOpacity = ffi "(function(sprite,opacity) {sprite.setOpacity(opacity);})"

updateSprite :: Ptr Sprite -> IO ()
updateSprite = ffi "(function(sprite) {sprite.update();})"
