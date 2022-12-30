import Data.List
import Data.List.Split

data Monkey = Monkey {
    items :: [Int],
    modf :: Int -> Int,
    checkf :: Int -> Bool,
    ift :: Int,
    iff :: Int,
    count :: Int
}

makefn :: String -> String -> (Int -> Int)
makefn s x
    | x == "old" = (flip (^)) 2
    | s == "*" = flip (*) (read x)
    | otherwise = flip (+) (read x)

parse :: [String] -> Monkey
parse [s_objs, s_op, s_check, s_iftrue, s_iffalse] = 
    let objs = map read $ drop 2 $ splitOn " " $ filter (/= ',') s_objs :: [Int]
        [op, ap] = drop 4 $ splitOn " " $ s_op :: [String]
        check = read . last $ splitOn " " $ s_check :: Int
        iftrue = read . last $ splitOn " " $ s_iftrue :: Int
        iffalse = read . last $ splitOn " " $ s_iffalse :: Int
    in Monkey objs (makefn op ap) (\x -> x `mod` check == 0) iftrue iffalse 0

throw :: (Int -> Int) -> [Monkey] -> Int -> [Monkey]
throw f monkeys i =
    let mo = (monkeys !! i)
        l = zip monkeys [0..length monkeys - 1]
        itrue = ift mo
        ifalse = iff mo
        new_items = map (f . modf mo) $ (items mo)
        (it_t, it_f) = partition (checkf mo) new_items
    in [if idx == i then Monkey [] (modf m) (checkf m) (ift m) (iff m) (count m + length new_items) else
        if idx == itrue then Monkey (items m ++ it_t) (modf m) (checkf m) (ift m) (iff m) (count m) else
        if idx == ifalse then Monkey (items m ++ it_f) (modf m) (checkf m) (ift m) (iff m) (count m) else
           m | (m, idx) <- l]

main :: IO ()
main = do
    all <- lines <$> getContents
    let inp = map tail $ splitOn [""] $ map (dropWhile (== ' ')) all
    let monkeys = map parse $ inp
    let round = (\x -> foldl (throw (flip div 3)) x [0..length x - 1])
    let round2 = (\x -> foldl (throw (flip mod 9699690)) x [0..length x - 1])
    print $ product . take 2 . reverse . sort . map count $ iterate round monkeys !! 20
    print $ product . take 2 . reverse . sort . map count $ iterate round2 monkeys !! 10000