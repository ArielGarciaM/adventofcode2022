import Data.List.Split
import Data.List

part1 :: [String] -> [Int] -> [String]
part1 st [cnt, from, to] = [if i == from then drop cnt $ st !! i else if i == to then moved ++ st !! i else st !! i | i <- [0 .. length st - 1]]
                             where moved = reverse . take cnt $ st !! from 

part2 :: [String] -> [Int] -> [String]
part2 st [cnt, from, to] = [if i == from then drop cnt $ st !! i else if i == to then moved ++ st !! i else st !! i | i <- [0 .. length st - 1]]
                             where moved = take cnt $ st !! from 

main :: IO ()
main = do
    all <- lines <$> getContents
    let [cancer, cancer2] = splitOn [""] all
    let stacks = map (filter ((/=) ' ') . init) $ transpose $ map ((map (flip (!!) 1)) . chunksOf 4) cancer
    let swaps = map parse cancer2 
                where parse s = [(arr !! 1), (arr !! 3) - 1, (arr !! 5) - 1] where arr = map read $ splitOn " " s
    print $ map head $ foldl part1 stacks swaps
    print $ map head $ foldl part2 stacks swaps

