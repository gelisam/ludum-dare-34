module Constants where


game_width :: Num a => a
game_width = 640

game_height :: Num a => a
game_height = 920

gravity :: Double
gravity = 0 -- 0.5

fps :: Num a => a
fps = 25


playerPixelsPerSecond :: Num a => a
playerPixelsPerSecond = 100

playerScale :: Double
playerScale = 1

playerImageWidth :: Num a => a
playerImageWidth = 117

playerImageHeight :: Num a => a
playerImageHeight = 300

playerWidth :: Double
playerWidth = playerScale * playerImageWidth

playerHeight :: Double
playerHeight = playerScale * playerImageHeight

playerInitialXPosition :: Num a => a
playerInitialXPosition = 300

playerInitialYPosition :: Num a => a
playerInitialYPosition = 780

fallingPlayerScale :: Double
fallingPlayerScale = 1

fallingPlayerImageWidth :: Num a => a
fallingPlayerImageWidth = 468

fallingPlayerImageHeight :: Num a => a
fallingPlayerImageHeight = 300

fallingPlayerWidth :: Double
fallingPlayerWidth = fallingPlayerScale * fallingPlayerImageWidth

fallingPlayerHeight :: Double
fallingPlayerHeight = fallingPlayerScale * fallingPlayerImageHeight

fallingPlayerInitialXPosition :: Num a => a
fallingPlayerInitialXPosition = 300

fallingPlayerInitialYPosition :: Num a => a
fallingPlayerInitialYPosition = 780



balloonScale :: Double
balloonScale = 0.3

balloonImageHeight :: Num a => a
balloonImageHeight = 475

balloonImageWidth :: Num a => a
balloonImageWidth = 192

balloonWidth :: Double
balloonWidth = balloonScale * balloonImageWidth

balloonHeight :: Double
balloonHeight = balloonScale * balloonImageHeight

birdPixelsPerSecond :: Num a => a
birdPixelsPerSecond = 100

birdScale :: Double
birdScale = 0.8

birdImageWidth :: Num a => a
birdImageWidth = 128

birdImageHeight :: Num a => a
birdImageHeight = 128

birdWidth :: Double
birdWidth = birdScale * birdImageWidth

birdHeight :: Double
birdHeight = birdScale * birdImageHeight
