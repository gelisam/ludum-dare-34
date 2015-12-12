{-# LANGUAGE OverloadedStrings #-}
module SpriteJS where

import Control.Monad
import Haste.DOM
import Haste.Foreign
import Haste.Prim

import JSRef


newtype Scene = Scene JSAny
newtype Surface = Surface JSAny
newtype Sprite = Sprite JSAny
newtype Layer = Layer JSAny
newtype SpriteList = SpriteList JSAny
newtype Input = Input JSAny
newtype Cycle = Cycle JSAny
newtype Ticker = Ticker JSAny


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


class CanHoldSprite a where
    newEmptySprite :: Ptr a -> IO (Ptr Sprite)

instance CanHoldSprite Scene where
    newEmptySprite = ffi "(function(scene) {return scene.Sprite(false);})"

instance CanHoldSprite Layer where
    newEmptySprite = ffi "(function(layer) {return layer.Sprite(false);})"

newSprite :: CanHoldSprite a => Ptr a -> JSString -> IO (Ptr Sprite)
newSprite parent image = do
    sprite <- newEmptySprite parent
    setSpriteImage sprite image
    return sprite

getSpriteX :: Ptr Sprite -> IO Int
getSpriteX = ffi "(function(sprite) {return sprite.x;})"

getSpriteY :: Ptr Sprite -> IO Int
getSpriteY = ffi "(function(sprite) {return sprite.y;})"

getSpriteWidth :: Ptr Sprite -> IO Int
getSpriteWidth = ffi "(function(sprite) {return sprite.w;})"

getSpriteHeight :: Ptr Sprite -> IO Int
getSpriteHeight = ffi "(function(sprite) {return sprite.h;})"

setSpriteImage :: Ptr Sprite -> JSString -> IO ()
setSpriteImage = ffi "(function(sprite,image) {sprite.loadImg(image);})"

setSpritePosition :: Ptr Sprite -> Int -> Int -> IO ()
setSpritePosition = ffi "(function(sprite,x,y) {sprite.position(x,y);})"

moveSpriteBy :: Ptr Sprite -> Int -> Int -> IO ()
moveSpriteBy = ffi "(function(sprite,dx,dy) {sprite.move(dx,dy);})"

-- in turns, not degrees nor radians
rotateSpriteBy :: Ptr Sprite -> Double -> IO ()
rotateSpriteBy = ffi "(function(sprite,turns) {sprite.rotate(2*Math.PI*turns);})"

setSpriteSize :: Ptr Sprite -> Int -> Int -> IO ()
setSpriteSize = ffi "(function(sprite,w,h) {sprite.size(w,h);})"

setSpriteXYScale :: Ptr Sprite -> Double -> Double -> IO ()
setSpriteXYScale = ffi "(function(sprite,xfactor,yfactor) {sprite.scale(xfactor,yfactor);})"

-- uniform scale
setSpriteScale :: Ptr Sprite -> Double -> IO ()
setSpriteScale sprite factor = setSpriteXYScale sprite factor factor

-- 1.0 for opaque, 0.0 for invisible
setSpriteOpacity :: Ptr Sprite -> Double -> IO ()
setSpriteOpacity = ffi "(function(sprite,opacity) {sprite.setOpacity(opacity);})"

xVelocity :: Ptr Sprite -> JSRef Double
xVelocity sprite = JSRef
  { readJSRef  = ffi "(function(sprite) {return sprite.xv;})" sprite
  , writeJSRef = ffi "(function(sprite,xv) {sprite.xv = xv;})" sprite
  }

yVelocity :: Ptr Sprite -> JSRef Double
yVelocity sprite = JSRef
  { readJSRef  = ffi "(function(sprite) {return sprite.yv;})" sprite
  , writeJSRef = ffi "(function(sprite,yv) {sprite.yv = yv;})" sprite
  }

applyXVelocity :: Ptr Sprite -> IO ()
applyXVelocity = ffi "(function(sprite) {return sprite.applyXVelocity();})"

applyYVelocity :: Ptr Sprite -> IO ()
applyYVelocity = ffi "(function(sprite) {return sprite.applyYVelocity();})"

applyVelocity :: Ptr Sprite -> IO ()
applyVelocity = ffi "(function(sprite) {return sprite.applyVelocity();})"

unapplyXVelocity :: Ptr Sprite -> IO ()
unapplyXVelocity = ffi "(function(sprite) {return sprite.reverseXVelocity();})"

unapplyYVelocity :: Ptr Sprite -> IO ()
unapplyYVelocity = ffi "(function(sprite) {return sprite.reverseYVelocity();})"

unapplyVelocity :: Ptr Sprite -> IO ()
unapplyVelocity = ffi "(function(sprite) {return sprite.reverseVelocity();})"

updateSprite :: Ptr Sprite -> IO ()
updateSprite = ffi "(function(sprite) {sprite.update();})"

collidesWith :: Ptr Sprite -> Ptr Sprite -> IO Bool
collidesWith = ffi "(function(sprite,sprite2) {return sprite.collidesWith(sprite2);})"

collidesWithArray :: Ptr Sprite -> [Ptr Sprite] -> IO (Maybe (Ptr Sprite))
collidesWithArray = ffi "(function(sprite,sprites) {return sprite.collidesWithArray(sprites) || null;})"

collidesWithSpriteList :: Ptr Sprite -> Ptr SpriteList -> IO (Maybe (Ptr Sprite))
collidesWithSpriteList = ffi "(function(sprite,sprites) {return sprite.collidesWithArray(sprites) || null;})"

isPointIn :: Ptr Sprite -> Int -> Int -> IO Bool
isPointIn = ffi "(function(sprite,x,y) {return sprite.isPointIn(x,y);})"


-- the documentation doesn't say what the name is for, is it even used?
newLayer :: Ptr Scene -> JSString -> IO (Ptr Layer)
newLayer = ffi "(function(scene,name) {return scene.Layer(name);})"


class HasDOM a where
    getDom :: Ptr a -> IO Elem

instance HasDOM Scene where
    getDom = ffi "(function(scene) {return scene.dom;})"

instance HasDOM Layer where
    getDom = ffi "(function(layer) {return layer.dom;})"

instance HasDOM Sprite where
    getDom = ffi "(function(sprite) {return sprite.dom;})"


newSpriteList :: IO (Ptr SpriteList)
newSpriteList = ffi "(function() {return sjs.SpriteList();})"

appendToSpriteList :: Ptr SpriteList -> Ptr Sprite -> IO ()
appendToSpriteList = ffi "(function(list,sprite) {list.add(sprite);})"

removeFromSpriteList :: Ptr SpriteList -> Ptr Sprite -> IO ()
removeFromSpriteList = ffi "(function(list,sprite) {list.remove(sprite);})"

nextSprite :: Ptr SpriteList -> IO (Maybe (Ptr Sprite))
nextSprite = ffi "(function(list) {return list.iterate() || null;})"

forEachSprite :: Ptr SpriteList -> (Ptr Sprite -> IO ()) -> IO ()
forEachSprite list body = do
    r <- nextSprite list
    forM_ r $ \sprite -> do
      body sprite
      forEachSprite list body


newInput :: Ptr Scene -> IO (Ptr Input)
newInput = ffi "(function(scene) {return scene.Input();})"

keydown :: Ptr Input -> IO Bool
keydown = ffi "(function(input) {return input.keydown;})"

mousedown :: Ptr Input -> IO Bool
mousedown = ffi "(function(input) {return input.mousedown;})"


newCycle :: Ptr Scene -> [(Int,Int,Int)] -> IO (Ptr Cycle)
newCycle = ffi "(function(scene,triplets) {return scene.Cycle(triplets);})"

appendToCycle :: Ptr Cycle -> Ptr Sprite -> IO ()
appendToCycle = ffi "(function(cycle,sprite) {cycle.addSprite(sprite);})"

updateCycle :: Ptr Cycle -> Ptr Ticker -> IO ()
updateCycle = ffi "(function(cycle,ticker) {cycle.next(ticker.lastTicksElapsed);})"


-- the Int argument is undocumented. maybe the target fps?
newTicker :: Ptr Scene -> Int -> (Ptr Ticker -> IO ()) -> IO (Ptr Ticker)
newTicker = ffi "(function(scene,fps,callback) {return scene.Ticker(fps,callback);})"

runTicker :: Ptr Ticker -> IO ()
runTicker = ffi "(function(ticker) {ticker.run();})"

pauseTicker :: Ptr Ticker -> IO ()
pauseTicker = ffi "(function(ticker) {ticker.pause();})"

resumeTicker :: Ptr Ticker -> IO ()
resumeTicker = ffi "(function(ticker) {ticker.resume();})"

getLastTicksElapsed :: Ptr Ticker -> IO Int
getLastTicksElapsed = ffi "(function(ticker) {return ticker.lastTicksElapsed;})"

getCurrentTick :: Ptr Ticker -> IO Int
getCurrentTick = ffi "(function(ticker) {return ticker.currentTick;})"
