import System.Random
import Control.Monad (replicateM)
import Data.Time.Clock.POSIX (getPOSIXTime)

randomList :: (Random a) => Int -> [a]
randomList seed = randoms(mkStdGen seed)

-- combine :: [Double] -> [Double] -> [[Double]]
-- combine xs ys = [xs, ys]

-- genRandomNumbers x: generate a list of x random numbers
genRandomNumbers :: (Random a, Integral a) => Int -> Int -> [a]
genRandomNumbers n seed = take n $ (randoms myGenerator) where
    myGenerator = mkStdGen seed

genRandomNumbersBetween :: Int -> Int -> (Int, Int) -> [Int]
genRandomNumbersBetween n seed (a, b) = take n $ (randomRs (a, b) myGenerator) where
    myGenerator = mkStdGen seed

main :: IO ()
main = do
    seed <- (round . (* 1000)) <$> getPOSIXTime 
    --putStrLn (show tenRandomNumbers)
    let weigth1 = replicate 30(take 64 (randomList seed :: [Double]))
    let weigth2 = replicate 10(take 30 (randomList seed :: [Double]))
    let bias1 = take 30 (randomList seed :: [Double])
    let bias2 = take 10 (randomList seed :: [Double])
    --putStrLn (show numbers)
    print $ bias2