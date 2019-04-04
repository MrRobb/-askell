
------------------------------------ INSERT

insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (x:l) e
    | e > x = x:(insert l e)
    | otherwise = (e:x:l)

------------------------------------ REMOVE

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:l) e
    | x == e = l
    | otherwise = x:(remove l e)

------------------------------------ MERGE

merge :: [Int] -> [Int] -> [Int]
merge l [] = l
merge [] l = l
merge (x:l1) (y:l2)
    | x < y = x:(merge l1 (y:l2))
    | otherwise = y:(merge (x:l1) l2)

------------------------------------ INSERTION SORT

isort :: [Int] -> [Int]
isort [] = []
isort (x:l) = insert (isort l) x

------------------------------------ SELECTION SORT

ssort :: [Int] -> [Int]
ssort [] = []
ssort l = x : (ssort (remove l x)) where x = minimum l

------------------------------------ MERGE SORT

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l =
    let
        (l1,l2) = splitAt (div (length l) 2) l
    in
        merge (msort l1) (msort l2)

------------------------------------ QUICKSORT

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:l) =
    let
        l1 = [ e | e <- l, e < x ]
        l2 = [ e | e <- l, e >= x ]
    in
        (qsort l1) ++ x:(qsort l2)

------------------------------------ GENERIC QUICKSORT

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:l) =
    let
        l1 = [ e | e <- l, e < x ]
        l2 = [ e | e <- l, e >= x ]
    in
        (genQsort l1) ++ x:(genQsort l2)

------------------------------------ TESTS

{-
main = do
    print(insert [10,20,30,40] 25)
    print(insert [10,20,30,40] 20)
    print(remove [6,4,3,5,2,3] 2)
    print(remove [6,4,3,5,2,3] 6)
    print(merge [1,2,5,7,8] [2,4,7,9])
    print(merge [] [])
    print(isort [6,5,2,5,6,8])
    print(ssort [6,5,2,5,6,8])
    print(msort [6,5,2,5,6,8])
    print(qsort [6,5,2,5,6,8])
-}
