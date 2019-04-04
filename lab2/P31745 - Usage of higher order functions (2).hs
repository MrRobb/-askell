
------------------------------------ FLATTEN

flatten :: [[Int]] -> [Int]
flatten ll = foldl (++) [] ll

------------------------------------ LENGTH

myLength :: String -> Int
myLength s = foldl (\n _ -> n + 1) 0 s

------------------------------------ REVERSE

myReverse :: [Int] -> [Int]
myReverse l = foldl (flip (:)) [] l

------------------------------------ COUNT

countIn :: [[Int]] -> Int -> [Int]
countIn ll x = map (length) (map (filter (== x)) ll)

------------------------------------ FIRST WORD

firstWord :: String -> String
firstWord s = takeWhile (/= ' ') (dropWhile (== ' ') s)

------------------------------------ TESTS

{-
main = do
    print(flatten [[1,2,3],[4,5],[6],[],[3,3]])
    print(myLength "Albert")
    print(myReverse [1..10])
    print(countIn [[3,2,3],[3],[], [2,2]] 3)
    print(firstWord "  Volem pa amb oli  ")
-}
