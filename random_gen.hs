import System.Random
import Control.Monad (replicateM)

randomList :: (Random a) => Int -> [a]
randomList seed = randoms(mkStdGen seed)

combine :: [Float] -> [Float] -> [[Float]]
combine xs ys = [xs, ys]

main :: IO ()
main = do 
    let y = replicate 30(take 64 (randomList 42 :: [Float]))
    let x = take 60 (randomList 42 :: [Float])
    print $ y