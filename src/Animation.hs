module Animation where

import Data.Fixed


-- In the style of Conal Elliott, an animation is a function
-- from time (in seconds) to value.
type Animation a = Double -> a


linear :: (Num a, Fractional a) => a -> a -> Animation a
linear initialValue unitsPerSecond t = initialValue + realToFrac t * unitsPerSecond

bounce :: (Real a, Fractional a) => (a, a) -> Animation a -> Animation (a, Bool)
bounce (lo, hi) anim t | x' < half_period = (lo + x', False)
                       | otherwise        = (lo + period - x', True)
  where
    half_period = hi - lo
    period = 2 * half_period
    x = anim t
    x' = (x - lo) `mod'` period
