{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeFamilies #-}
module SpriteJS where

import Haste.DOM
import Haste.Foreign
import Haste.Prim
import Control.Monad
import Control.Monad.Extra
import JSRef


newtype Scene = Scene JSAny
newtype Surface = Surface JSAny
newtype Sprite = Sprite JSAny
newtype Layer = Layer JSAny
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
    newEmptySprite :: a -> IO (Ptr Sprite)
    getScene :: a -> IO (Ptr Scene)

instance CanHoldSprite (Ptr Scene) where
    newEmptySprite = ffi "(function(scene) {return scene.Sprite(false);})"
    getScene = return

instance CanHoldSprite (Ptr Layer) where
    newEmptySprite = ffi "(function(layer) {return layer.Sprite(false);})"
    getScene = getLayerScene

newSprite :: CanHoldSprite a
          => a
          -> JSString -- ^ image file
          -> IO (Ptr Sprite)
newSprite parent image = do
    sprite <- newEmptySprite parent
    writeJSRef (spriteImage sprite) image
    return sprite


class SpriteLike a where
    type UpdateParam a
    
    rawSprites       :: a -> [NormalSprite]
    collisionSprites :: a -> [NormalSprite]
    
    spriteImage    :: a -> JSRef JSString
    spriteOffset   :: a -> JSRef (Double, Double) -- the image is cropped to this position
    spriteSize     :: a -> JSRef (Double, Double) -- and size
    spriteScale    :: a -> JSRef (Double, Double) -- then scaled
    spriteAngle    :: a -> JSRef Double -- in turns, not degrees nor radians
    spriteOpacity  :: a -> JSRef Double -- 1.0 for opaque, 0.0 for invisible
    
    spritePosition :: a -> JSRef (Double, Double)
    spriteVelocity :: a -> JSRef (Double, Double)
    
    updateSprite :: a -> UpdateParam a -> IO ()

    removeSprite :: a -> IO ()

type NormalSprite = Ptr Sprite

instance SpriteLike (Ptr Sprite) where
    type UpdateParam (Ptr Sprite) = ()
    
    rawSprites       = return
    collisionSprites = return

    spriteImage sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return sprite.src;})" sprite
      , writeJSRef = ffi "(function(sprite,image) {sprite.loadImg(image);})" sprite
      }
    spriteOffset sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return [sprite.xoffset, sprite.yoffset];})" sprite
      , writeJSRef = ffi "(function(sprite,o) {sprite.offset(o[0], o[1]);})" sprite
      }
    spriteSize sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return [sprite.w, sprite.h];})" sprite
      , writeJSRef = ffi "(function(sprite,s) {sprite.size(s[0], s[1]);})" sprite
      }
    spriteScale sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return [sprite.xscale, sprite.yscale];})" sprite
      , writeJSRef = ffi "(function(sprite,s) {sprite.scale(s[0], s[1]);})" sprite
      }
    spriteAngle sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return sprite.angle / (2*Math.PI);})" sprite
      , writeJSRef = ffi "(function(sprite,angle) {sprite.setAngle(angle * (2*Math.PI));})" sprite
      }
    spriteOpacity sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return sprite.opacity;})" sprite
      , writeJSRef = ffi "(function(sprite,opacity) {sprite.setOpacity(opacity);})" sprite
      }
    spritePosition sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return [sprite.x, sprite.y];})" sprite
      , writeJSRef = ffi "(function(sprite,p) {sprite.position(p[0], p[1]);})" sprite
      }
    spriteVelocity sprite = JSRef
      { readJSRef  = ffi "(function(sprite) {return [sprite.xv, sprite.yv];})" sprite
      , writeJSRef = ffi "(function(sprite,v) {sprite.xv = v[0]; sprite.yv = v[1];})" sprite
      }
    
    updateSprite sprite () = ffi "(function(sprite) {sprite.update();})" sprite

    removeSprite sprite = do
      _ <- forM (rawSprites sprite) $ \s -> rawRemove s
      return ()  


spriteXOffset :: SpriteLike a => a -> JSRef Double
spriteXOffset sprite = JSRef
  { readJSRef  = fst <$> readJSRef (spriteOffset sprite)
  , writeJSRef = \x -> do
      (_,y) <- readJSRef (spriteOffset sprite)
      writeJSRef (spriteOffset sprite) (x,y)
  }

spriteYOffset :: SpriteLike a => a -> JSRef Double
spriteYOffset sprite = JSRef
  { readJSRef  = snd <$> readJSRef (spriteOffset sprite)
  , writeJSRef = \y -> do
      (x,_) <- readJSRef (spriteOffset sprite)
      writeJSRef (spriteOffset sprite) (x,y)
  }

spriteWidth :: SpriteLike a => a -> JSRef Double
spriteWidth sprite = JSRef
  { readJSRef  = fst <$> readJSRef (spriteSize sprite)
  , writeJSRef = \w -> do
      (_,h) <- readJSRef (spriteSize sprite)
      writeJSRef (spriteSize sprite) (w,h)
  }

spriteHeight :: SpriteLike a => a -> JSRef Double
spriteHeight sprite = JSRef
  { readJSRef  = snd <$> readJSRef (spriteSize sprite)
  , writeJSRef = \h -> do
      (w,_) <- readJSRef (spriteSize sprite)
      writeJSRef (spriteSize sprite) (w,h)
  }

spriteXScale :: SpriteLike a => a -> JSRef Double
spriteXScale sprite = JSRef
  { readJSRef  = fst <$> readJSRef (spriteScale sprite)
  , writeJSRef = \sx -> do
      (_,sy) <- readJSRef (spriteScale sprite)
      writeJSRef (spriteScale sprite) (sx,sy)
  }

spriteYScale :: SpriteLike a => a -> JSRef Double
spriteYScale sprite = JSRef
  { readJSRef  = snd <$> readJSRef (spriteScale sprite)
  , writeJSRef = \sy -> do
      (sx,_) <- readJSRef (spriteScale sprite)
      writeJSRef (spriteScale sprite) (sx,sy)
  }

-- uniform scale
setSpriteScale :: SpriteLike a => a -> Double -> IO ()
setSpriteScale sprite factor = writeJSRef (spriteScale sprite) (factor, factor)

spriteXPosition :: SpriteLike a => a -> JSRef Double
spriteXPosition sprite = JSRef
  { readJSRef  = fst <$> readJSRef (spritePosition sprite)
  , writeJSRef = \x -> do
      (_,y) <- readJSRef (spritePosition sprite)
      writeJSRef (spritePosition sprite) (x,y)
  }

spriteYPosition :: SpriteLike a => a -> JSRef Double
spriteYPosition sprite = JSRef
  { readJSRef  = snd <$> readJSRef (spritePosition sprite)
  , writeJSRef = \y -> do
      (x,_) <- readJSRef (spritePosition sprite)
      writeJSRef (spritePosition sprite) (x,y)
  }

spriteXVelocity :: SpriteLike a => a -> JSRef Double
spriteXVelocity sprite = JSRef
  { readJSRef  = fst <$> readJSRef (spriteVelocity sprite)
  , writeJSRef = \xv -> do
      (_,yv) <- readJSRef (spriteVelocity sprite)
      writeJSRef (spriteVelocity sprite) (xv,yv)
  }

spriteYVelocity :: SpriteLike a => a -> JSRef Double
spriteYVelocity sprite = JSRef
  { readJSRef  = snd <$> readJSRef (spriteVelocity sprite)
  , writeJSRef = \yv -> do
      (xv,_) <- readJSRef (spriteVelocity sprite)
      writeJSRef (spriteVelocity sprite) (xv,yv)
  }

applyVelocity   :: SpriteLike a => a -> IO ()
applyVelocity   sprite = do
    (x ,y ) <- readJSRef (spritePosition sprite)
    (xv,yv) <- readJSRef (spriteVelocity sprite)
    let x' = x + xv
    let y' = y + yv
    writeJSRef (spritePosition sprite) (x',y')

unapplyVelocity :: SpriteLike a => a -> IO ()
unapplyVelocity sprite = do
    (x ,y ) <- readJSRef (spritePosition sprite)
    (xv,yv) <- readJSRef (spriteVelocity sprite)
    let x' = x - xv
    let y' = y - yv
    writeJSRef (spritePosition sprite) (x',y')

explode :: SpriteLike a => a -> IO [NormalSprite]
explode sprite = do
  xxs <- forM (rawSprites sprite) $ \s -> rawExplode s
  return $ concat xxs

rawRemove :: Ptr Sprite -> IO ()
rawRemove = ffi "(function(sprite){ return sprite.remove(); })"

rawExplode :: Ptr Sprite -> IO [NormalSprite]
rawExplode = ffi "(function(sprite){ return sprite.explode4(); })"

rawCollidesWith :: Ptr Sprite -> Ptr Sprite -> IO Bool
rawCollidesWith = ffi "(function(sprite,sprite2) {return sprite.collidesWith(sprite2);})"

rawCollidesWithArray :: Ptr Sprite -> [Ptr Sprite] -> IO (Maybe (Ptr Sprite))
rawCollidesWithArray = ffi "(function(sprite,sprites) {return sprite.collidesWithArray(sprites) || null;})"

rawIsPointIn :: Ptr Sprite -> Int -> Int -> IO Bool
rawIsPointIn = ffi "(function(sprite,x,y) {return sprite.isPointIn(x,y);})"

collidesWith :: (SpriteLike a, SpriteLike b) => a -> b -> IO Bool
collidesWith sprite1 sprite2 = orM [ rawCollidesWith s1 s2
                                   | s1 <- collisionSprites sprite1
                                   , s2 <- collisionSprites sprite2
                                   ]

collidesWithList :: (SpriteLike a, SpriteLike b) => a -> [b] -> IO Bool
collidesWithList sprite sprites = orM [ rawCollidesWith s1 s2
                                      | s1 <- collisionSprites sprite
                                      , s2 <- concatMap collisionSprites sprites
                                      ]


-- the documentation doesn't say what the name is for, is it even used?
newLayer :: Ptr Scene -> JSString -> IO (Ptr Layer)
newLayer = ffi "(function(scene,name) {return scene.Layer(name);})"

getLayerScene :: Ptr Layer -> IO (Ptr Scene)
getLayerScene = ffi "(function(layer) {return layer.scene;})"


class HasDOM a where
    getDom :: a -> IO Elem

instance HasDOM (Ptr Scene) where
    getDom = ffi "(function(scene) {return scene.dom;})"

instance HasDOM (Ptr Layer) where
    getDom = ffi "(function(layer) {return layer.dom;})"

instance HasDOM (Ptr Sprite) where
    getDom = ffi "(function(sprite) {return sprite.dom;})"


newInput :: Ptr Scene -> IO (Ptr Input)
newInput = ffi "(function(scene) {return scene.Input();})"

keydown :: Ptr Input -> IO Bool
keydown = ffi "(function(input) {return input.keydown;})"

leftdown :: Ptr Input -> IO Bool
leftdown = ffi "(function(input) {return input.keyboard.left;})"

rightdown :: Ptr Input -> IO Bool
rightdown = ffi "(function(input) {return input.keyboard.right;})"

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
