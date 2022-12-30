import Data.List

approach :: (Int, Int) -> (Int, Int) -> (Int, Int)
approach h t
    | abs dx <= 1 && abs dy <= 1 = t
    | abs dx == 0 = (fst t, snd t + dy `div` 2)
    | abs dy == 0 = (fst t + dx `div` 2, snd t)
    | otherwise = (fst t + dx `div` abs dx, snd t + dy `div` abs dy)
    where [dx, dy] = [fst h - fst t, snd h - snd t]

follow :: (a -> a -> a) -> a -> [a] -> [a]
follow f h [] = [h]
follow f h (x : xs) = h : follow f (f h x) xs

moverope :: [(Int, Int)] -> String -> [(Int, Int)]
moverope ((hx, hy) : t) dir
    | dir == "R" = follow approach (hx + 1, hy) t
    | dir == "L" = follow approach (hx - 1, hy) t
    | dir == "U" = follow approach (hx, hy + 1) t
    | dir == "D" = follow approach (hx, hy - 1) t

apply :: [String] -> [(Int, Int)] -> [(Int, Int)]
apply [] l = [last l]
apply (c : xs) l = last l : apply xs (moverope l c)

main :: IO ()
main = do
    all <- lines <$> getContents
    let inst = concat $ map (\[c, n] -> take (read n) $ repeat c) $ map words all
    print $ length . nub $ apply inst $ take 2 $ repeat (0, 0)
    print $ length . nub $ apply inst $ take 10 $ repeat (0, 0)