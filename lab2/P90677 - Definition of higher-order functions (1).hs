
------------------------------------ FOLDL

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ xi [] = xi
myFoldl fn xi (x:l) = (myFoldl fn (fn xi x) l)

------------------------------------ FOLDR

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ xi [] = xi
myFoldr fn xi (x:l) = (fn x (myFoldr fn xi l))

------------------------------------ ITERATE

myIterate :: (a -> a) -> a -> [a]
myIterate fn x = x:(myIterate fn (fn x))

------------------------------------ UNTIL

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil cond fn = val
    where
        val x
            | cond x = x
            | otherwise = val (fn x)

------------------------------------ MAP

myMap :: (a -> b) -> [a] -> [b]
myMap fn l = myFoldr (\x1 l1 -> (fn x1):l1) [] l

------------------------------------ FILTER

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn l = myFoldr (\x1 l1 -> if fn x1 then x1:l1 else l1) [] l

------------------------------------ ALL

myAll :: (a -> Bool) -> [a] -> Bool
myAll fn l = and (myMap fn l)

------------------------------------ ANY

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn l = or (myMap fn l)

------------------------------------ ZIP

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x1:l1) (x2:l2) = (x1,x2):(myZip l1 l2)

------------------------------------ ZIPWITH

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith fn l1 l2 = myMap (\(x1,x2) -> fn x1 x2) (myZip l1 l2)

------------------------------------ TESTS

{-
main = do
    print(myFoldl (+) 1 [1..5])
    print(myFoldr (+) 1 [1..5])
    print(take 10 $ myIterate (*2) 1)
    print(myUntil (>100) (*2) 1)
    print(myMap ("la "++) ["joana", "mireia"])
    print(myFilter odd [1..10])
    print(myAll odd [1,3,5,3,1])
    print(myAll (== True) (repeat False))
    print(myAny odd [2,4,6,8,10])
    print(myZip [1..4] [1..3])
    print(myZipWith (+) [1..4] [1..3])
-}
