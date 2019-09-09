import System.Random
import Control.Monad (replicateM)
import Data.Time.Clock.POSIX (getPOSIXTime)

randomList :: (Random a) => Int -> [a]
randomList seed = randoms(mkStdGen seed)

-- combine :: [Double] -> [Double] -> [[Double]]
-- combine xs ys = [xs, ys]

main :: IO ()
main = do
    let tenRandomNumbers = replicateM 1 randomIO :: IO [Int]
    --putStrLn (show tenRandomNumbers)
    let weigth1 = replicate 30(take 64 (randomList 40 :: [Double]))
    let weigth2 = replicate 10(take 30 (randomList 44 :: [Double]))
    let bias1 = take 30 (randomList 45 :: [Double])
    let bias2 = take 10 (randomList 46 :: [Double])
    print $ weigth1