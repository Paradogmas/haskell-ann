import System.IO
import Data.List
import Control.Monad (liftM)
import Control.Monad (replicateM)

parse_lines :: [String] -> (Int, Int, [Int], [[Int]])
parse_lines (mn_line : ks_line : matrix_lines) = (m, n, ks, matrix)
    where [m, n] = read_ints    mn_line
          ks     = read_ints    ks_line
          matrix = parse_matrix matrix_lines

read_ints :: String -> [Int]
read_ints = map read . words

parse_matrix :: [String] -> [[Int]]
parse_matrix lines = parse_matrix' lines []
    where parse_matrix' []       acc = reverse acc
          parse_matrix' (l : ls) acc = parse_matrix' ls $ (read_ints l) : acc

parse_file :: FilePath -> IO (Int, Int, [Int], [[Int]])
parse_file filename = do
    file_lines <- (liftM lines . readFile) filename
    return $ parse_lines file_lines

main = do

    data_set <- readFile "out.txt"
    putStrLn  data_set

    -- target_contents <- readFile "target.txt"
    -- print $ target_contents
    -- let a = map read $ words target_contents :: [Int]
    -- print $ a
    
