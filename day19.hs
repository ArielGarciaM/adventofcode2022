import Data.List
import Data.List.Split
import Debug.Trace

backtrack :: [Int] -> [Int] -> [([Int], Int)] -> Int -> Int
backtrack _ _ _ 1 = 0
backtrack robots resources bp step =
    let big = [opt | opt <- init bp, all (==True) $ zipWith (>=) resources (fst opt), (snd opt > 0 || (fst opt) !! 0 < step), step > 3, (step > 4 || snd opt == 2), (step > 5 || snd opt /= 1), (robots !! 0 <= 3 || snd opt /= 0), (robots !! 1 < (fst $ bp !! 2) !! 1 || snd opt /= 1), (robots !! 2 < (fst $ bp !! 3) !! 2 || snd opt /= 2)]
        options = if length big > 0 && (robots !! 0 == 4 || (robots !! 0 == 3 && resources !! 0 >= 5)) then big else ([0, 0, 0], -1) : big
        cash = if (all (==True) $ zipWith (>=) resources (fst $ last bp)) then (step - 1) + backtrack robots (zipWith (+) robots (zipWith (-) resources (fst $ last bp))) bp (step - 1) else 0
    in if step == 2 then cash else
        if (step <= 10 && robots !! 2 == 0) then 0 else
        maximum $ cash : [backtrack [robots !! i + fromEnum (snd opt == i) | i <- [0..2]] (zipWith (+) robots (zipWith (-) resources (fst opt))) bp (step - 1) | opt <- options]

main :: IO ()
main = do
    all <- lines <$> getContents
    let parsebp x = [([read $ x !! 6, 0, 0], 0),
                     ([read $ x !! 12, 0, 0], 1),
                     ([read $ x !! 18, read $ x !! 21, 0], 2),
                     ([read $ x !! 27, 0, read $ x !! 30], 3)] :: [([Int], Int)]
        bps = map (parsebp . splitOn " ") $ all
    print $ bps
    print $ map (\bp -> backtrack [1, 0, 0] [0, 0, 0] bp 32) bps
    --print $ sum $ zipWith (*) [3,1,2,6,0,13,6,3,1,2,7,0,2,0,1,1,2,0,0,0,0,0,6,3,1,3,1,1,5,4] [1..]