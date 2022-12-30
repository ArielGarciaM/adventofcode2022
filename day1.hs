import Control.Monad
import Data.List

main = do
    lines <- getLines
    print (sum (take 3 (reverse (sort (result lines 0)))))
  
result :: [String] -> Int -> [Int]
result [] n = [n]
result (l:xs) n = do
    let s = l
    if s /= "" then result xs (n + read s)
    else (n : result xs 0)

getLines :: IO [String]
getLines = lines <$> getContents

