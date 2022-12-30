import Data.List

part1 :: [Char] -> Int -> Int
part1 xs x = if (length . nub . take 4 $ xs) == 4 then x + 4 else part1 (tail xs) x + 1


part2 :: [Char] -> Int -> Int
part2 xs x = if (length . nub . take 14 $ xs) == 14 then x + 14 else part2 (tail xs) x + 1

main :: IO ()
main = do
    all <- lines <$> getContents
    print $ part1 (head all) 0
    print $ part2 (head all) 0