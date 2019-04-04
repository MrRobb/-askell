
------------------------------------ COUNT IF

countIf :: (Int -> Bool) -> [Int] -> Int
countIf fn l = length (filter fn l)

------------------------------------ MULTI MAP 1

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l lfn = map (\fn -> map fn l) lfn

------------------------------------ MULTI MAP 2

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l lfn = map (\x -> [ fn x | fn <- lfn ]) l

------------------------------------ FILTER AND FOLD

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl fnFilter fnFold x l = foldl fnFold x (filter fnFilter l)

------------------------------------ SORT INSERT

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert _ [] x = [x]
insert fn (y:l) x
    | not (fn y x) = x:y:l
    | otherwise = y:(insert fn l x)

------------------------------------ INSERTION SORT

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort fn l = foldr (\x lx -> insert fn lx x) [] l

------------------------------------ TESTS

{-
main = do
    print(countIf (>5) [1..10])
    print(pam [1,2,3] [(+1),(*2),(^2)])
    print(pam2 [1,2,3] [(+1),(*2),(^2)])
    print(filterFoldl even (*) 1 [4,7,2,4,9,3])
    print(insert (<) [1,4,6,9,12] 8)
    print(insertionSort (>) [4,5,2,3,1,3])
-}
