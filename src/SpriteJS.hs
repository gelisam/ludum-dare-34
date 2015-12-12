{-# LANGUAGE OverloadedStrings #-}
module SpriteJS where

import Haste.DOM
import Haste.Foreign
import Haste.Prim


newtype Scene = Scene JSAny
newtype Surface = Surface JSAny
newtype Sprite = Sprite JSAny
newtype Layer = Layer JSAny


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

updateSprite :: Ptr Sprite -> IO ()
updateSprite = ffi "(function(sprite) {sprite.update();})"


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


rest :: Int -> Int -> Ptr Scene -> Ptr Layer -> Ptr Layer -> Ptr Sprite -> IO ()
rest = ffi
    "(function(game_width,game_height,scene,back,front,score) {                                                                                                                 \
    \     var bottom = scene.Sprite('img/crate.png', {layer:back, w:game_width, h:64, x:0, y:game_height-64});                     \
    \     bottom.update();                                                                                                         \
    \     var elements = sjs.SpriteList();                                                                                         \
    \     elements.add(bottom);                                                                                                    \
    \                                                                                                                              \
    \     var player = scene.Sprite('img/character.png', front);                                                                   \
    \     player.position(40, 200);                                                                                                \
    \     player.size(28, 52);                                                                                                     \
    \     player.scale(-1, 1);                                                                                                     \
    \                                                                                                                              \
    \     var input  = scene.Input();                                                                                              \
    \                                                                                                                              \
    \     var cycle = scene.Cycle([[3, 3, 5],                                                                                      \
    \                                [33, 3, 5],                                                                                   \
    \                                [63, 3, 5],                                                                                   \
    \                                [93, 3, 5],                                                                                   \
    \                                [123, 3, 5],                                                                                  \
    \                                [153, 3, 5],                                                                                  \
    \                                [183, 3, 5]]);                                                                                \
    \     cycle.addSprite(player);                                                                                                 \
    \                                                                                                                              \
    \     var virtual_player_x = player.x;                                                                                         \
    \     var player_xv = 2.5;                                                                                                     \
    \     var score_count = 0;                                                                                                     \
    \                                                                                                                              \
    \     function paint() {                                                                                                       \
    \                                                                                                                              \
    \         var gravity = 0.5;                                                                                                         \
    \         player.yv += gravity;                                                                                                \
    \         player.applyXVelocity();                                                                                             \
    \         if(player.collidesWithArray(elements)) {                                                                             \
    \             ticker.pause();                                                                                                  \
    \             alert(\"Game over!\");                                                                                           \
    \             return;                                                                                                          \
    \         }                                                                                                                    \
    \                                                                                                                              \
    \         player.applyYVelocity();                                                                                             \
    \         if(player.collidesWithArray(elements)) {                                                                             \
    \             player.reverseYVelocity();                                                                                       \
    \             player.yv = 0;                                                                                                   \
    \             if(input.mousedown || input.keydown) {                                                                           \
    \                 player.yv = -10;                                                                                             \
    \             }                                                                                                                \
    \         } else {                                                                                                             \
    \             if(input.mousedown || input.keydown) {                                                                           \
    \                 player.yv -= 0.2;                                                                                            \
    \             }                                                                                                                \
    \         }                                                                                                                    \
    \                                                                                                                              \
    \         player.update();                                                                                                     \
    \                                                                                                                              \
    \         var el;                                                                                                              \
    \         var need_to_create_plateform = true;                                                                                 \
    \         while(el = elements.iterate()) {                                                                                     \
    \             el.xv = -player_xv;                                                                                              \
    \             el.applyVelocity();                                                                                              \
    \             el.update();                                                                                                     \
    \                                                                                                                              \
    \             if(el.isPointIn(game_width, game_height-20)) {                                                                   \
    \                 need_to_create_plateform = false;                                                                            \
    \             }                                                                                                                \
    \                                                                                                                              \
    \             if(el.x + el.w < 0) {                                                                                            \
    \                 elements.remove(el)                                                                                          \
    \             }                                                                                                                \
    \         }                                                                                                                    \
    \                                                                                                                              \
    \         if(need_to_create_plateform && Math.random() < 0.1) {                                                                \
    \             var height = 32 + (Math.random() * 96);                                                                          \
    \             var width = 64 + (Math.random() * 128);                                                                          \
    \             var bottom = scene.Sprite('img/crate.png', {layer:back, w:width, h:height, x:game_width, y:game_height-height}); \
    \             bottom.update();                                                                                                 \
    \             elements.add(bottom);                                                                                            \
    \         }                                                                                                                    \
    \                                                                                                                              \
    \         cycle.next(ticker.lastTicksElapsed);                                                                                 \
    \                                                                                                                              \
    \         player_xv += 0.002;                                                                                                  \
    \         score_count += 0.08;                                                                                                 \
    \         score.dom.innerHTML = 'Score '+Math.round(score_count);                                                              \
    \                                                                                                                              \
    \         if(player.y > game_height) {                                                                                         \
    \             ticker.pause();                                                                                                  \
    \             alert(\"Game over\");                                                                                              \
    \             return;                                                                                                          \
    \         }                                                                                                                    \
    \     };                                                                                                                       \
    \                                                                                                                              \
    \     var ticker = scene.Ticker(25, paint);                                                                                    \
    \     ticker.run();                                                                                                            \
    \})"
