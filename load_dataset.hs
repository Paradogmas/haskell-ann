import System.IO
import Data.List

takeTrainNaive :: Int -> [a] -> [a]
takeTrainNaive n = reverse . take n . reverse 

takeTestNaive :: Int -> [a] -> [a]
takeTestNaive n = take n

main = do

    data_set <- readFile "dataset.txt"
    print $ data_set

    target_contents <- readFile "target.txt"
    -- print $ target_contents
    let a = map read $ words target_contents :: [Int]
    -- splitting the data
    let x_train = takeTrainNaive 719 a
    let x_test = takeTestNaive 1078 a
    print $ x_train
