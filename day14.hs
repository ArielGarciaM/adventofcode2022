import Data.List
import Data.List.Split

approach :: (Int, Int) -> (Int, Int) -> (Int, Int)
approach (x1, y1) (x2, y2)
    | x1 == x2 = (x1, y1 + (y2 - y1) `div` abs (y2 - y1))
    | y1 == y2 = (x1 + (x2 - x1) `div` abs (x2 - x1), y1)

trace :: [(Int, Int)] -> [(Int, Int)]
trace [p] = [p]
trace (p : q : l) = if p == q then trace (q : l) else p : trace (approach p q : q : l)

dropsand :: (Int, Int) -> [String] -> [String]
dropsand (x, y) grid
    | (y < n && grid !! (y + 1) !! x == '.') = dropsand (x, y + 1) grid
    | (y < n && x > 0 && grid !! (y + 1) !! (x - 1) == '.') = dropsand (x - 1, y + 1) grid 
    | (y < n && x < m && grid !! (y + 1) !! (x + 1) == '.') = dropsand (x + 1, y + 1) grid
    | otherwise = [[if (xx, yy) == (x, y) then 'o' else grid !! yy !! xx | xx <- [0 .. m]] | yy <- [0 .. n]]
    where n = (length grid) - 1
          m = (length (head grid)) - 1

flood :: [String] -> Int -> [String]
flood grid x = if (length . filter (== 'o') $ last grid) > 0 then grid else flood (dropsand (x, 0) grid) x

makerow :: String -> String -> String
makerow s cur = let n = length s - 1 in
            "." ++ [if cur !! x == '.' && (s !! (x - 1) == 'o' || s !! x == 'o' || s !! (x + 1) == 'o') then 'o' else cur !! x | x <- [1 .. n - 1]] ++ "."

main :: IO ()
main = do
    all <- lines <$> getContents

    let l = nub . concat $ map (trace . map (\s -> read $ "(" ++ s ++ ")" :: (Int, Int)) . splitOn " -> ") all
        xmin = minimum . map fst $ l
        xmax = maximum . map fst $ l
        ymin = minimum . map snd $ l
        ymax = maximum . map snd $ l
        n = ymax + 1
        m = xmax - xmin + 500
        dropx = 500 - xmin + 250
        fl = map (\(x, y) -> (x - xmin + 250, y)) l
        grid = [[if (x, y) `elem` fl then '#' else '.' | x <- [0 .. m]] | y <- [0 .. n]]

    let ini = [if x == dropx then 'o' else '.' | x <- [0 .. m]]
    let maxgrid = scanl makerow ini (tail grid)
    --print $ (length . filter (== 'o') . concat $ flood grid dropx) - 1
    print $ length . filter (== 'o') . concat $ maxgrid