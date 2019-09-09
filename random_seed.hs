import System.Random
import Control.Monad (replicateM)
import Data.Time.Clock.POSIX (getPOSIXTime)

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
    -- -- Generate 10 random numbers with a millisecond timestamp
    -- -- as the seed and display them to the console.
    -- putStrLn "Print 10 random numbers"
    -- let numbers = genRandomNumbers 10 seed :: [Int]
    -- putStrLn (show numbers)
    -- seed <- (round .(* 1000)) <$> getPOSIXTime
    -- putStrLn "Print 10 random numbers between 1 and 10"
    let numbers =  take 1 (genRandomNumbersBetween 10 seed (1, 10) :: [Int])
    putStrLn (show numbers)
    return ()