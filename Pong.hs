import Haste
import Haste.Graphics.Canvas
import Data.IORef


data GameState = GameState{
    ballPos :: Point, -- position of ball
    ballSpeed :: Point, -- how far will ball move in a single update
    paddlePos:: Double, -- start position of paddle on x axis
    score  :: Int
  }

width, height,ballRadius, paddleWidth, paddleHeight :: Double
width = 500 -- width of canvas
height = 600 -- height of canvas
ballRadius = 5 --radius of ball
paddleHeight = 5 -- height of paddle
paddleWidth = 150 -- width of paddle
halfWidth = width / 2 -- well, half the width
halfHeight = height / 2 --also half the height

scoreLabel :: String
scoreLabel = "Score: "

initialState :: GameState
initialState = GameState{
        ballPos = (20, 20),
        ballSpeed = (8, 10),
        paddlePos = (width / 2) - 75, --position around center of canvas
        score = 0
}

mkCanvas width height = do
        canvas <- newElem "canvas"
        setProp canvas "width" (show width)
        setProp canvas "height" (show height)
        setStyle canvas "display" "block"
        setStyle canvas "border" "1px solid black"
        setStyle canvas "margin" "0px auto 0 auto"
        setStyle canvas "backgroundColor" "black"
        return canvas

circle :: Point -> Double -> Shape() -- draw a circle at given Point with given radius
rect   :: Point -> Point  -> Shape() -- draw a rectangle between two points
color  :: Color -> Picture() -> Picture() -- draw the given picture using the specified color

type Point = (Double, Double)

white :: Picture () -> Picture () 
white = color (RGB 255 255 255) -- or whichever color you like

ball :: Point -> Picture ()
ball pt = white $ do
  fill $ circle pt ballRadius

paddle :: Rect -> Picture () 
paddle (Rect x1 y1 x2 y2) = white $ do
  fill $ rect (x1, y1) (x2, y2)

drawText :: Point -> String -> Picture ()
drawText point msg = white $ do 
  text point msg

gamePicture :: GameState -> Picture ()
gamePicture state = do
  ball $ ballPos state -- ball position from `state`
  let x1 = paddlePos state -- paddle start position
      x2 = x1 + paddleWidth -- end position of paddle
  paddle $ Rect x1 0 x2 paddleHeight -- top paddle
  paddle $ Rect x1 (height - paddleHeight) x2 height -- bottom paddle
  font "20px italic Monospace" $ drawText (30,50) $ scoreLabel ++ show (score state) -- write the score onto the canvas

renderState :: Canvas -> GameState -> IO ()
renderState canvas state = render canvas $ do
        gamePicture state

main :: IO ()
main = do
  canvasElem <- mkCanvas width height
  addChild canvasElem documentBody
  Just canvas <- getCanvas canvasElem
  renderState canvas initialState
