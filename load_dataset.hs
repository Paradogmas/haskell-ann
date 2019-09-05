import System.IO

main = do
    let file_dataset = "dataset.txt"
    contents <- readFile file_dataset
    putStrLn contents
