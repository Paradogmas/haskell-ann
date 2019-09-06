import System.IO
import Data.List

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

deriv_f :: Double -> Double
deriv_f x = sigmoid x * (1 - sigmoid x )

takeTrainNaive :: Int -> [a] -> [a]
takeTrainNaive n = reverse . take n . reverse 

takeTestNaive :: Int -> [a] -> [a]
takeTestNaive n = take n

main = do
    
    target_contents <- readFile "target.txt"
    let a = map read $ words target_contents :: [Int]
    
    -- splitting the data
    let y_train = takeTrainNaive 719 a
    let y_test = takeTestNaive 1078 a

    let x = sigmoid 5

    print x
