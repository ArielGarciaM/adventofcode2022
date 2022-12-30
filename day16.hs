import Data.List
import Data.List.Split
import Data.Bits
import Data.Maybe
import Data.Array
import qualified Data.Map as Map

iteratedp :: Int -> [Int] -> Array Int [Int] -> Array Int Int -> Array Int (Array Int Int) -> Array Int (Array Int Int)
iteratedp n good edges values dp =
    let k = length good
        isgood = listArray (0, n - 1) [x `elem` good | x <- [0 .. n - 1]] :: Array Int Bool
        goodindex = listArray (0, n - 1) [if isgood ! x then (fromJust $ findIndex (== x) good) else -1 | x <- [0 .. n - 1]] :: Array Int Int
        states = 2 ^ k
        opener mask v = if (isgood ! v && mask `testBit` (goodindex ! v)) then values ! (mask `clearBit` (goodindex ! v)) + dp ! (mask `clearBit` (goodindex ! v)) ! v else -1000000000
        edger mask v = values ! mask + maximum [dp ! mask ! u | u <- edges ! v]
    in array (0, 2^k - 1) [(mask, array (0, n - 1) [(v, max (opener mask v) (edger mask v)) | v <- [0 .. n - 1]]) | mask <- [0 .. states - 1]]

main :: IO ()
main = do
    all <- lines <$> getContents
    let parse l = (\x -> (x !! 1, read . init . tail . dropWhile (/= '=') $ x !! 4, map (filter (/= ',')) $ drop 9 x) :: (String, Int, [String])) $ splitOn " " l
        n = length all
        clean = map parse all
        conv = Map.fromList $ zip (map (\(x, y, z) -> x) clean) [0..]
        real = map (\(x, y, z) -> (fromJust $ Map.lookup x $ conv, y, map (\w -> fromJust $ Map.lookup w $ conv) z) :: (Int, Int, [Int])) clean
        src = fromJust $ Map.lookup "AA" $ conv
        goodtuples = filter (\(x, y, z) -> y /= 0) real
        good = map (\(x, y, z) -> x) goodtuples
        goodvalues = map (\(x, y, z) -> y) goodtuples
        k = length good
        values = listArray (0, 2^k - 1) [sum [if ((mask .&. (bit i) :: Int) == 0) then 0 else goodvalues !! i | i <- [0..k-1]] | mask <- [0 .. 2^k - 1]]
        edges = listArray (0, n - 1) $ map (\(x, y, z) -> z) real
        dp = [[if (mask == 0 && v == src) then 0 else -1000000000 | v <- [0 .. n - 1]] | mask <- [0 .. 2^k - 1]]
        dparr = array (0, 2^k - 1) [(mask, array (0, n - 1) [(v, dp !! mask !! v) | v <- [0 .. n - 1]]) | mask <- [0 .. 2^k - 1]]
        final = iterate' (iteratedp n good edges values) dparr !! 30
        maskmx = listArray (0, 2^k - 1) $ map (maximum . elems) $ elems final
        smm = array (0, 2^k - 1) [(x, maximum $ (maskmx ! x) : [smm ! (x `clearBit` b) | b <- [0 .. k - 1], (x `testBit` b)]) | x <- [0 .. 2^k - 1]] :: Array Int Int
    print $ smm ! (2^k - 1)
    print $ maximum [maskmx ! x + smm ! (2^k - 1 - x) | x <- [0 .. 2^k - 1]]
    --print $ maximum $ map maximum $ iterate' (iteratedp n good edges values) dp !! 30