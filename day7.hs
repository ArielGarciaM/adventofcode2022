import Data.List.Split
import Data.List

part1 :: [String] -> [Int] -> [Int] -> [Int]
part1 [] [x] full = x : full
part1 [] path full = part1 [] ((path !! 0 + path !! 1) : drop 2 path) (head path : full)
part1 (ss : xs) path full =
    let s = words ss in
    if (head s == "$") then
        if (s !! 1 == "ls") then part1 xs path full
        else if (s !! 2 == "..") then part1 xs ((path !! 0 + path !! 1) : drop 2 path) (head path : full)
        else if (s !! 2 == "/") then part1 xs path full
        else part1 xs (0 : path) full
    else if (head s == "dir") then part1 xs path full
    else
        part1 xs (read (head s) + head path : tail path) full

main :: IO ()
main = do
    all <- lines <$> getContents
    let sizes = part1 all [0] []
    print $ sum . filter (<= 100000) $ sizes
    print $ minimum . filter (>= (head sizes - 40000000)) $ sizes

