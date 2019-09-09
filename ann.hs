import System.IO
import Data.List
import Control.Monad (liftM)
import Control.Monad (replicateM)
import Control.Monad.Cont
import Data.Typeable
import System.Random
import Data.Ord

y_to_vec :: Double -> Double -> [Double] -> [Double]
y_to_vec _ 10 _ = []
y_to_vec v i res
    | i == v = 1.0 : (y_to_vec v (i+1) res)
    | otherwise = 0.0 : (y_to_vec v (i+1) res)

randomList :: (Random a) => Int -> [a]
randomList seed = randoms(mkStdGen seed)

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip[0..]

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

takeTestNaive :: Int -> [a] -> [a]
takeTestNaive n = reverse . take n . reverse 

takeTrainNaive :: Int -> [a] -> [a]
takeTrainNaive n = take n

takeTestMatrixNaive :: Int -> [[a]] -> [[a]]
takeTestMatrixNaive n = reverse . take n . reverse 

takeTrainMatrixNaive :: Int -> [[a]] -> [[a]]
takeTrainMatrixNaive n = take n

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

-- remove_bracket :: [[Double]] -> [Double]
remove_bracket x = do
    return x!!0

feed_forwardZ::[Double]->[[Double]]->[Double]->[Double]
feed_forwardZ h w b= do
    let z_temp = calc_zn h w b
    z<-remove_bracket z_temp
    return z

main = do
    -- setting neural network structure
    let nn_structure = [64, 30, 10] -- input, hidden, output

    putStrLn ("\n============= Estrutura =============")
    putStrLn ("Entrada: 64 nodes")
    putStrLn ("Oculta: 30 nodes")
    putStrLn ("Saida: 10 nodes")

    -- reading and parsing dataset
    let b = parse_file "dataset.txt"
    data_set <- b

    putStrLn ("\n\n============= Exemplo de dado do dataset =============\n")
    print (data_set !! 1)

    -- reading and parsing target
    target_contents <- readFile "target.txt"
    let a = map read $ words target_contents :: [Int]
    
    putStrLn ("\n\n============= Escalando dados =============")
    -- scalling the data
    let matrixC = concat data_set 

    putStr ("\nMedia: ")
    let x_mean = mean matrixC
    print (x_mean)

    putStr ("Desvio Padrao: ")
    let x_stdev = stdev matrixC x_mean
    print (x_stdev)

    let x_scale = [ scaleData y x_mean x_stdev | y <- data_set ]

    putStrLn ("\n\n============= Exemplo de dado escalado do dataset =============\n")
    print (x_scale !! 1)
    
    putStrLn ("\nFazendo split...")
    -- splitting the data
    let y_train = takeTrainNaive 1078 a
    let y_test = takeTestNaive 719 a

    let x_train = takeTrainMatrixNaive 1078 x_scale
    let x_test = takeTestMatrixNaive 719 x_scale

    putStrLn ("Feito")

    putStrLn ("\nInicializando delta W e delta b...")
    -- initializing tri values
    let tri_W = init_tri_W_values
    let tri_b = init_tri_b_values
    putStrLn ("Feito")

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

    -- Generate random weight and bias
    let weigth1 = replicate 30(take 64 (randomList 40 :: [Double]))
    let weigth2 = replicate 10(take 30 (randomList 44 :: [Double]))
    let bias1 = take 30 (randomList 45 :: [Double])
    let bias2 = take 10 (randomList 46 :: [Double])
    
    -- initializing random matrix
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
    let z_out = map deriv_f b1
    let h_out = map sigmoid z_out
    let y = [0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
    let delta2 = out_layer_delta y h_out z_out

    -- calculating hidden layer
    let z_l = map deriv_f b1
    let w1_t = transpose w1
    let w2_t = transpose w2
    let delta_plus_1 = b2
    let delta3 = hidden_delta delta_plus_1 w1_t z_l
    
    putStrLn ("\nFazendo predicao...")
    -- predicting number
    y_pred <- forM [0 .. 718] $ \i -> do
        let h1 = x_test!!i
        let z2 = feed_forwardZ h1 w1 b1
        let h2 = map sigmoid z2
        let z3 = feed_forwardZ h2 w2 b2
        let h3 = map sigmoid z3
        let position = maxIndex h3
        return position
    
    putStrLn ("Feito")
    putStrLn ("============= Vetor predito =============\n")
    print (y_pred)

    putStrLn ("\n============= Resposta correta =============\n")
    print (y_test)

    putStr ("\nAcuracia: ")
    print $ accuracy y_pred y_test