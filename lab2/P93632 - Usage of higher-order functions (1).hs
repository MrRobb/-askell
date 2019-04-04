
------------------------------------ EQUALS

eql :: [Int] -> [Int] -> Bool
eql l1 l2 = (length l1 == length l2) && (and (zipWith (==) l1 l2))

------------------------------------ PRODUCT LIST

prod :: [Int] -> Int
prod l = foldl (*) 1 l

------------------------------------ PRODUCT EVEN LIST

prodOfEvens :: [Int] -> Int
prodOfEvens l = prod (filter (even) l)

------------------------------------ POWERS OF 2

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

------------------------------------ DOT PRODUCT

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = foldl (+) 0 (zipWith (*) l1 l2)

------------------------------------ TESTS

{-
main = do
    print(eql [1,2,3] [1,2,3])
    print(eql [1,2,3] [3,2,1])
    print(eql [1,2,3] [1,2,3,4])
    print(prod [2,10,5])
    print(prodOfEvens [2,10,5])
    print(take 5 powersOf2)
    print(scalarProduct [2.0,1.0,5.0] [3.0,2.0,2.0])
-}
