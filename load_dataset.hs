import System.IO
import Data.List
import Control.Monad (liftM)
import Control.Monad (replicateM)
import Data.Typeable
import System.Random

sumList :: [Double] -> Double
sumList [] = 0
sumList (h:t) = h + sumList t

sumProd :: [Double] -> [Double] -> Double
sumProd xs ys = sumList $ zipWith (*) xs ys

np_dot :: [[Double]] -> [Double] -> [Double]
np_dot w_l delta = [(sumProd h delta) | h <- w_l]

listToListOfLists [] = []
listToListOfLists (h:t) = [h] : listToListOfLists t

out_layer_delta :: [Double] -> [Double] -> [Double] -> [Double]
out_layer_delta y h_out z_out = zipWith (*) (zipWith (-) h_out y) z_out

hidden_delta :: [Double] -> [[Double]] -> [Double] -> [Double]
hidden_delta delta_plus_1 w_l z_l = zipWith (*) fst z_l
    where fst = np_dot w_l delta_plus_1

init_tri_W_values :: ([[Double]], [[Double]])
init_tri_W_values = (zero 30 64, zero 10 30)

init_tri_b_values :: ([Double], [Double])
init_tri_b_values = (replicate 30 0, replicate 10 0)

sumMatrices :: [[Double]] -> [[Double]] -> [[Double]]
sumMatrices a b = [(zipWith (+) ha hb) | ha <- a, hb <- b]

zero :: Int -> Int -> [[Double]]
zero x y = replicate y (replicate x 0)

variancia :: [Double] -> Double -> [Double]
variancia xs m = [(x_new-m)**2 | x_new<-xs]

stdev :: [Double] -> Double -> Double
stdev xs m = sqrt(sum (variancia xs m)  / fromIntegral (length xs -1))

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

scaleData :: [Double] -> Double -> Double -> [Double]
scaleData x mean stdev = [(x_new-mean) / stdev | x_new<-x]

parse_lines :: [String] -> ([[Double]])
parse_lines (mn_line : ks_line : matrix_lines) = (matrix)
    where matrix = parse_matrix matrix_lines

read_ints :: String -> [Double]
read_ints = map read . words

parse_matrix :: [String] -> [[Double]]
parse_matrix lines = parse_matrix' lines []
    where parse_matrix' []       acc = reverse acc
          parse_matrix' (l : ls) acc = parse_matrix' ls $ (read_ints l) : acc

parse_file :: FilePath ->  IO ([[Double]])
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

takeTrainMatrixNaive :: Int -> [[a]] -> [[a]]
takeTrainMatrixNaive n = reverse . take n . reverse 

takeTestMatrixNaive :: Int -> [[a]] -> [[a]]
takeTestMatrixNaive n = take n

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


-- setting and initializing weight and bias
random_float :: [Double] -> [Double]
random_float [] = []
random_float i = (head i)/1000:random_float (tail i)

--random_generator :: Int -> Double
random_generator n = do
    g <- getStdGen
    let ns = take n $ randomRs (0, 1000) g
    let f_ns = random_float ns
    return f_ns

-- calculating feed_forward
calc_zn :: [Double] -> [[Double]] -> [Double] -> [Double]
calc_zn x w b = zipWith (+) fst b
    where fst = np_dot w x

--remove_bracket :: [[Double]] -> [Double] NÃO COLOCAR
remove_bracket x = do
    return x!!0

feed_forward::[Double]->[[Double]]->[[Double]]->[Double]->[Double]->([Double], [Double])
feed_forward x w1 w2 b1 b2 = do
    let z2_temp = calc_zn x w1 b1
    z2<-remove_bracket z2_temp
    let h1 = x
    let h2 = sigmoid z2
    let z3_temp = calc_zn [h2] w2 b2
    z3<-remove_bracket z3_temp
    let h3 = sigmoid z3
    return z2, z3

main = do
    -- setting neural network structure
    let nn_structure = [64, 30, 10] -- input, hidden, output

    -- reading and parsing dataset
    let b = parse_file "dataset.txt"
    data_set <- b

    -- reading and parsing target
    target_contents <- readFile "target.txt"
    let a = map read $ words target_contents :: [Int]
    
    -- scalling the data
    let matrixC = concat data_set 
    let x_mean = mean matrixC
    let x_stdev = stdev matrixC x_mean
    let x_scale = [ scaleData y x_mean x_stdev | y <- data_set ]
    
    -- splitting the data
    let y_train = takeTrainNaive 719 a
    let y_test = takeTestNaive 1078 a

    let x_train = takeTrainMatrixNaive 719 x_scale
    let x_test = takeTestMatrixNaive 1078 x_scale

    -- initializing tri values
    let tri_W = init_tri_W_values
    let tri_b = init_tri_b_values

    -- feed forward
    -- let z

    -- testing area
    let c = dot [[1, 2],[3, 4]] [[-3, -8, 3],[-2,  1, 4]]
    let x = map deriv_f [5, 1, 3, 4]
    let y = accuracy [1, 2, 3, 5, 4, 5, 6, 6, 7, 6, 7, 8, 8, 7, 8, 9] [1, 2, 4, 5, 4, 5, 6,6, 7,6, 7, 8,8, 7, 8, 9]

    let azero = zero 3 3
    let hdf = replicate 3 (replicate 3 2)

    let res = sumMatrices azero hdf

    -- end of testing area

    -- initializing random matrix
    let w = random_generator 10
    --
    let w1_temp = parse_file "W1.txt"
    w1 <- w1_temp
    let w2_temp = parse_file "W2.txt"
    w2 <- w2_temp
    let b1_temp = parse_file "b1.txt"
    b1_t <- b1_temp
    let b1 = head b1_t
    let b2_temp = parse_file "b2.txt"
    b2_t <- b2_temp
    let b2 = head b2_t

    -- calculating out layer
    let z_out = map deriv_f[ 10.97622676, 13.05608545, 12.15755343, 14.79820165, 13.81879473, 13.56181247, 14.68160928, 13.90150112,  9.81737804, 11.30229501]
    let h_out = [0.9999829 , 0.99999786, 0.99999475, 0.99999963, 0.999999, 0.99999871, 0.99999958, 0.99999908, 0.99994551, 0.99998766]
    let y = [0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
    let out_layer = out_layer_delta y h_out z_out

    -- calculating hidden layer
    let z_l = map deriv_f[ 2.04104047, -0.40176612,  1.1203371 , -0.12657657,  2.27092157, -0.56148751, -0.60633191, -1.67640165,  0.5778819,  2.1530739, 1.2299639 ,  1.69531853,  2.74005396, -1.49828843,  0.85051871,-1.89140984, -0.02889386, -0.25561791, -0.50356901, -1.93193024,0.22219219, -2.38848939,  0.673749  ,  1.68621401, -1.26106785,1.18627974,  1.06056405,  1.99820895, -0.19181241, -0.52293671]
    
    --let w1 = transpose w1
    --let w2 = transpose w2

    let delta_plus_1 = [-1.21164785e-08,  3.66611946e-04,  2.52650336e-04,  3.91324499e-04, 2.87442999e-04,  1.07657421e-04,  1.58559847e-04,  1.42728374e-04, 1.07571735e-04,  4.86890380e-04]
    --let hidden_layer = hidden_delta delta_plus_1 w_l z_l
    let x_temp = x_train!!1
    --print $ length x_temp
    let ff = feed_forward x_temp w1 w2 b1 b2
    print $ ff