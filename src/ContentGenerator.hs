{-# OPTIONS_GHC -fno-warn-orphans #-}
module ContentGenerator where

import Entities


instance (Num a, Num b) => Num (a, b) where
    (x1,y1) + (x2,y2) = (x1+x2, y1+y2)
    (x1,y1) * (x2,y2) = (x1*x2, y1*y2)
    abs (x,y) = (abs x, abs y)
    signum (x,y) = (signum x, signum y)
    negate (x,y) = (negate x, negate y)
    fromInteger n = (fromInteger n, fromInteger n)


stairs :: Num a
       => Int -- ^ length
       -> a -- ^ offset
       -> a -- ^ gap
       -> [a]
stairs n x0 dx = [x0 + fromIntegral i * dx | i <- [0..n-1]]

birdStairs :: Int -- ^ length
           -> Double -- ^ x offset
           -> Double -- ^ y offset
           -> Double -- ^ horizontal gap
           -> Double -- ^ vertical gap
           -> [OffScreenBird]
birdStairs n x0 y0 dx dy = map (uncurry OffScreenBird)
                         $ stairs n (x0,y0) (dx,dy)

generateBirds :: IO [OffScreenBird]
generateBirds = return (birdStairs 50 0 100 100 (-200))
