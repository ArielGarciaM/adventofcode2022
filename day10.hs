import Data.List
import Data.List.Split

main :: IO ()
main = do
    all <- lines <$> getContents
    let ops = foldl1 (++) $ map words all
    let signals = scanl (\tot s -> if s `elem` ["noop", "addx"] then tot else tot + read s) 1 ops
    print $ sum $ zipWith (*) signals [if x `mod` 40 == 19 then x + 1 else 0 | x <- [0..219]]
    putStr $ unlines $ map (zipWith (\p q -> if abs (p - q) <= 1 then '#' else '.') [0..39]) $ chunksOf 40 signals


    