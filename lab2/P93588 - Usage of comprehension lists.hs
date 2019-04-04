
------------------------------------ MAP

myMap :: (a -> b) -> [a] -> [b]
myMap fn l = [ fn x | x <- l ]

------------------------------------ FILTER

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn l = [ x | x <- l, fn x ]

------------------------------------ ZIPWITH

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith fn l1 l2 = [ fn x1 x2 | (x1, x2) <- zip l1 l2 ]

------------------------------------ COMBINATIONS

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify l1 l2 = [ (x1, x2) | x1 <- l1, x2 <- l2, (mod x1 x2) == 0 ]

------------------------------------ FACTORS

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], (mod n x) == 0 ]

------------------------------------ TESTS

{-
main = do
    print(myMap (*2) [1..5])
    print(myFilter odd [1..50])
    print(myZipWith (*) [1..4] [1..4])
    print(thingify [1..6] [1..3])
    print(factors 24)
-}
