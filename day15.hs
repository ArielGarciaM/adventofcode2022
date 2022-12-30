import Data.List
import Data.List.Split

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

segmentOn :: Int -> [Int] -> (Int, Int)
segmentOn y [x1, y1, x2, y2] = let mdis = dist (x1, y1) (x2, y2)
                                   vdis = abs (y - y1)
                                   rdis = mdis - vdis
                                in (x1 - rdis, x1 + rdis)

sweep :: [(Int, Int)] -> Int -> Int -> Int -> Int
sweep [] l r _ = r - l + 1
sweep ((x, y) : xs) l r b
    | x > y = sweep xs l r b
    | x <= r = sweep xs (min b l) (min b (max r y)) b
    | otherwise = r - l + 1 + sweep xs (min b x) (min b y) b

main :: IO ()
main = do
    all <- lines <$> getContents
    
    let clean = map (map read . zipWith ($) [takeWhile (/= ','), takeWhile (/= ':'), takeWhile (/= ','), id] . tail . splitOn "=") all :: [[Int]]
        segs = sort . map (segmentOn 2000000) $ clean
        beacons = nub $ map (\[_,_,x,y] -> (x, y)) clean
        mn = fst $ head segs
    print $ (sweep segs (mn - 1) (mn - 2) 1000000000) - (length $ filter (\(x, y) -> y == 2000000) beacons)

    let l = [(y, sweep (sort . map (segmentOn y) $ clean) 0 (-1) 4000000) | y <- [0..4000000]]
        yy = fst . head . filter (\(x, y) -> y == 4000000) $ l
        xx = head [x | x <- [0..4000000], foldl (&&) True (map (\[x1, y1, x2, y2] -> dist (x1, y1) (x2, y2) < dist (x1, y1) (x, yy)) clean) == True]
    print $ 4000000 * xx + yy