import Data.List
import Data.Char

prio :: Char -> Int
prio c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27

part1 :: String -> Int
part1 s = prio . head $ intersect first second
    where (first, second) = splitAt (length s `div` 2) s

part2 :: [String] -> Int
part2 [] = 0
part2 (a : b : c : xs) = (prio . head $ foldl1 intersect [a, b, c]) + part2 xs

main :: IO ()
main = do
    all <- lines <$> getContents
    print $ sum [part1 a | a <- all]
    print $ part2 all

