import Data.List

snatodec :: String -> Int
snatodec x = decode $ reverse x where
    decode "" = 0
    decode (x : xs) = (5 * decode xs) + case x of
        '=' -> -2
        '-' -> -1
        '0' -> 0
        '1' -> 1
        '2' -> 2

dectosna :: Int -> String
dectosna x = reverse $ decode x where
    decode 0 = ""
    decode x = let y = x `div` 5 in
        case x `mod` 5 of
            0 -> '0' : decode y
            1 -> '1' : decode y
            2 -> '2' : decode y
            3 -> '=' : decode (y + 1)
            4 -> '-' : decode (y + 1)

main :: IO ()
main = do
    all <- lines <$> getContents
    print $ dectosna . sum . map snatodec $ all