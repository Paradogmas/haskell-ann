import System.IO
import Data.List

stdev :: [Double] -> Double
stdev xs = sqrt . average . map ((^2) . (-) axs) $ xs
           where average = (/) <$> sum <*> realToFrac . length
                 axs     = average xs



sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

deriv_f :: Double -> Double
deriv_f x = sigmoid x * (1 - sigmoid x)

takeTrainNaive :: Int -> [a] -> [a]
takeTrainNaive n = reverse . take n . reverse 

takeTestNaive :: Int -> [a] -> [a]
takeTestNaive n = take n

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

    target_contents <- readFile "target.txt"
    let a = map read $ words target_contents :: [Int]
    
    -- splitting the data
    let y_train = takeTrainNaive 719 a
    let y_test = takeTestNaive 1078 a

    let x = map deriv_f [5, 1, 3, 4]

    let y = accuracy [1, 2, 3, 5, 4, 5, 6, 6, 7, 6, 7, 8, 8, 7, 8, 9] [1, 2, 4, 5, 4, 5, 6,6, 7,6, 7, 8,8, 7, 8, 9]

    print x
