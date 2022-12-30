import Data.List
import Data.List.Split

tokenize :: String -> String -> Int -> [String]
tokenize "" "" _ = []
tokenize "" token _ = [reverse token]
tokenize (c : s) token level
    | (c == ',' && level == 0) = (reverse token) : tokenize s "" 0
    | (c == '[') = tokenize s (c : token) (level + 1)
    | (c == ']') = tokenize s (c : token) (level - 1)
    | otherwise = tokenize s (c : token) level

cmpPacket :: String -> String -> Ordering
cmpPacket s t
    | (hs /= '[' && ht /= '[') = let [ns, nt] = [read s :: Int, read t :: Int] in compare ns nt
    | (hs /= '[') = cmpPacket ("[" ++ s ++ "]") t
    | (ht /= '[') = cmpPacket s ("[" ++ t ++ "]")
    | otherwise = let [ss, tt] = [drop 1 $ init s, drop 1 $ init t] in cmpList (tokenize ss "" 0) (tokenize tt "" 0)
    where hs = head s
          ht = head t

cmpList :: [String] -> [String] -> Ordering
cmpList [] [] = EQ
cmpList [] _ = LT
cmpList _ [] = GT
cmpList (x : xs) (y : ys) = let res = cmpPacket x y in if res /= EQ then res else cmpList xs ys

main :: IO ()
main = do
    all <- lines <$> getContents
    let l = splitOn [""] all
    --print $ sum $ zipWith (\x y -> if x == LT then y else 0) (map (\[x, y] -> cmpPacket x y) l) [1..]

    let l2 = filter (/= "") all ++ ["[[2]]", "[[6]]"]
    print $ product $ zipWith (\x y -> if x `elem` ["[[2]]", "[[6]]"] then y else 1) (sortBy cmpPacket l2) [1..]