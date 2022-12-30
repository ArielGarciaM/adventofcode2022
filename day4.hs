import Data.List.Split

part1 :: [[Int]] -> Bool
part1 [[a, b], [c, d]] = [max a c, min b d] `elem` [[a, b], [c, d]]

part2 :: [[Int]] -> Bool
part2 [[a, b], [c, d]] = max a c <= min b d

parse :: String -> [[Int]]
parse s = map ((map read) . splitOn "-") $ splitOn "," s :: [[Int]]

main :: IO ()
main = do
    all <- lines <$> getContents
    print $ sum $ map (fromEnum . part1 . parse) all
    print $ sum $ map (fromEnum . part2 . parse) all

