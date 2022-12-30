import Data.List

value :: Char -> Int
value 'X' = 1
value 'Y' = 2
value 'Z' = 3

svalue :: Char -> Int
svalue 'X' = 0
svalue 'Y' = 3
svalue 'Z' = 6

result :: String -> Int
result s = value (last s) + 3 + (if s `elem` ["A Y", "B Z", "C X"] then 3 else 0) + (if s `elem` ["A Z", "B X", "C Y"] then -3 else 0)

sresult :: String -> Int
sresult s = svalue (last s) + (if s `elem` ["A Y", "B X", "C Z"] then 1 else 0) + (if s `elem` ["B Y", "C X", "A Z"] then 2 else 0) + (if s `elem` ["C Y", "A X", "B Z"] then 3 else 0)

main :: IO ()
main = do
    all <- lines <$> getContents
    print (sum [result a | a <- all])
    print (sum [sresult a | a <- all])