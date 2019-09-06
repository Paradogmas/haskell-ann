import System.IO
import Data.List
import Control.Monad (liftM)
import Control.Monad (replicateM)

variancia :: [Double] -> Double -> [Double]
variancia xs m = [(x_new-m)**2 | x_new<-xs]

stdev :: [Double] -> Double -> Double
stdev xs m = sqrt(sum (variancia xs m)  / fromIntegral (length xs -1))

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

scaleData :: [Double] -> Double -> Double -> [Double]
scaleData x mean stdev = [(x_new-mean) / stdev | x_new<-x]

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

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

deriv_f :: Double -> Double
deriv_f x = sigmoid x * (1 - sigmoid x)

takeTrainNaive :: Int -> [a] -> [a]
takeTrainNaive n = reverse . take n . reverse 

takeTestNaive :: Int -> [a] -> [a]
takeTestNaive n = take n

foldlZipWith::(a -> b -> c) -> (d -> c -> d) -> d -> [a] -> [b]  -> d
foldlZipWith _ _ u [] _          = u
foldlZipWith _ _ u _ []          = u
foldlZipWith f g u (x:xs) (y:ys) = foldlZipWith f g (g u (f x y)) xs ys
 
foldl1ZipWith::(a -> b -> c) -> (c -> c -> c) -> [a] -> [b] -> c
foldl1ZipWith _ _ [] _          = error "First list is empty"
foldl1ZipWith _ _ _ []          = error "Second list is empty"
foldl1ZipWith f g (x:xs) (y:ys) = foldlZipWith f g (f x y) xs ys
 
multAdd::(a -> b -> c) -> (c -> c -> c) -> [[a]] -> [[b]] -> [[c]]
multAdd f g xs ys = map (\us -> foldl1ZipWith (\u vs -> map (f u) vs) (zipWith g) us ys) xs
 
dot:: Num a => [[a]] -> [[a]] -> [[a]]
dot xs ys = multAdd (*) (+) xs ys
    
accuracy :: [Int] -> [Int] -> Double
accuracy list1 list2 
    | length list1 /= length list2 = 0
    | length list1 == length list2 = fromIntegral (absoluteAccuracy list1 list2) / fromIntegral (length list1)

absoluteAccuracy :: [Int] -> [Int] -> Int
absoluteAccuracy [] _ = 0
absoluteAccuracy (h:t) (h2:t2) 
    | h == h2 = 1 + absoluteAccuracy t t2
    | otherwise = 0 + absoluteAccuracy t t2


main = do
    let nn_structure = [64, 30, 10]

    let c = dot [[1, 2],[3, 4]] [[-3, -8, 3],[-2,  1, 4]]

    target_contents <- readFile "target.txt"
    let a = map read $ words target_contents :: [Int]
    
    -- scalling the data
    let x_mean = mean [1, 2, 3, 5, 4, 5, 6, 6, 7, 6, 7, 8, 8, 7, 8, 9]
    let x_stdev = stdev [1, 2, 3, 5, 4, 5, 6, 6, 7, 6, 7, 8, 8, 7, 8, 9] x_mean
    let x_scale = scaleData [1, 2, 3, 5, 4, 5, 6, 6, 7, 6, 7, 8, 8, 7, 8, 9] x_mean x_stdev
    
    -- splitting the data
    let y_train = takeTrainNaive 719 a
    let y_test = takeTestNaive 1078 a

    let x = map deriv_f [5, 1, 3, 4]

    let y = accuracy [1, 2, 3, 5, 4, 5, 6, 6, 7, 6, 7, 8, 8, 7, 8, 9] [1, 2, 4, 5, 4, 5, 6,6, 7,6, 7, 8,8, 7, 8, 9]

    print c
