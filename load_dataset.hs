import System.IO
import Data.List

main = do

    data_set <- readFile "dataset.txt"
    print $ data_set

    target_contents <- readFile "target.txt"
    -- print $ target_contents
    let a = map read $ words target_contents :: [Int]
    print $ a
