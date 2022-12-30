import Data.List

visible :: Ord a => [a] -> [Bool]
visible l = True : [maximum (take x l) < (l !! x) | x <- [1..length l - 1]]

dist :: Ord a => [a] -> [Int]
dist l = let pos x = [y | y <- [0 .. x - 1], (l !! y) >= (l !! x)]
         in [if length (pos x) > 0 then x - last (pos x) else x | x <- [0 .. length l - 1]]

main :: IO ()
main = do
    all <- lines <$> getContents
    --let left = map visible all
    --let right = map (reverse . visible) $ map reverse all
    --let up = transpose . map visible $ transpose all
    --let down = transpose . map reverse . map visible $ map reverse $ transpose all
    --let full = [foldl1 (zipWith (||)) $ map (!! x) [left, right, up, down] | x <- [0..length left - 1]]
    --print $ sum $ map (sum . map fromEnum) full
    let left = map dist all
    let right = map (reverse . dist) $ map reverse all
    let up = transpose . map dist $ transpose all
    let down = transpose . map reverse . map dist $ map reverse $ transpose all
    let full = [foldl1 (zipWith (*)) $ map (!! x) [left, right, up, down] | x <- [0..length left - 1]]
    print $ maximum $ map maximum full