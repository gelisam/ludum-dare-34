-- Haste's documentation says to use the random module, but doing so gives the error
-- "ghczuwrapperZC0ZCbaseZCSystemziCPUTimeZCgetrusage is not defined" at runtime.
-- 
-- Haste implements a few random primitives, but is missing the primitives I've
-- reimplemented here.
module Random where

import Data.IORef
import Haste
import System.IO.Unsafe


data RandomState = RandomState
  { currentSeed :: Seed
  , currentSkipRate :: Int
  }

{-# NOINLINE randomStateRef #-}
randomStateRef :: IORef RandomState
randomStateRef = unsafePerformIO $ do
    seed <- newSeed
    newIORef (RandomState seed 1)


randomRIO :: Random a => (a, a) -> IO a
randomRIO range = do
    RandomState seed skipRate <- readIORef randomStateRef
    let (x, seed') = randomR range seed
    writeIORef randomStateRef (RandomState seed' skipRate)
    return x

randomRIOs :: Random a => (a, a) -> IO [a]
randomRIOs range = do
    independentInt <- randomRIO (0, maxBound)
    let independentSeed = mkSeed independentInt
    return $ randomRs range independentSeed
