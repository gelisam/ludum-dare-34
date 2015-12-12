-- Haste's documentation says to use the random module, but doing so gives the error
-- "ghczuwrapperZC0ZCbaseZCSystemziCPUTimeZCgetrusage is not defined" at runtime.
-- 
-- Haste implements a few random primitives, but they're not as flexible as the random
-- module, so I'm reimplementing the random module on top of Haste's primitives.
module Random where

import Data.IORef
import Data.Tuple
import qualified Haste as H
import System.IO.Unsafe
import qualified System.Random as R


{-# NOINLINE stdGenRef #-}
stdGenRef :: IORef R.StdGen
stdGenRef = unsafePerformIO $ do
    seed <- H.newSeed
    -- (minBound, maxBound) causes an exception to be thrown
    newIORef $ R.mkStdGen $ fst $ H.randomR (0, maxBound) seed

withStdGen :: (R.StdGen -> (a, R.StdGen)) -> IO a
withStdGen f = atomicModifyIORef' stdGenRef (swap . f)


randomRIO :: R.Random a => (a, a) -> IO a
randomRIO = withStdGen . R.randomR

randomIO :: R.Random a => IO a
randomIO = withStdGen R.random

randomIOs :: R.Random a => IO [a]
randomIOs = do
    x <- randomIO
    g <- readIORef stdGenRef
    let xs = R.randoms g
    return (x:xs)
