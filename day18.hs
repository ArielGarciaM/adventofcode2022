import Data.List
import Data.List.Split
import qualified Data.Set as S

neighboring :: (Int, Int, Int) -> [(Int, Int, Int)]
neighboring (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1 .. 1], dy <- [-1 .. 1], dz <- [-1 .. 1], abs dx + abs dy + abs dz == 1]

dfs :: (Int, Int, Int) -> S.Set (Int, Int, Int) -> S.Set (Int, Int, Int) -> S.Set (Int, Int, Int)
dfs u visited cubes =
    let next cube = [(x, y, z) | (x, y, z) <- neighboring cube, minimum [x, y, z] >= -1,
                                              maximum [x, y, z] <= 25, S.notMember (x, y, z) cubes]
    in foldl (\visited v -> if S.member v visited then visited else dfs v visited cubes)
        (S.union visited (S.singleton u)) (next u) 

main :: IO ()
main = do
    all <- lines <$> getContents
    let cubes = S.fromList $ map (\s -> read $ "(" ++ s ++ ")") all :: S.Set (Int, Int, Int)
        freefaces cube = S.size $ S.difference (S.fromList $ neighboring cube) cubes
        outside = dfs (-1, -1, -1) S.empty cubes
        outfaces cube = S.size $ S.intersection (S.fromList $ neighboring cube) outside
    print $ sum $ map freefaces $ S.elems cubes
    print $ sum $ map outfaces $ S.elems cubes