myLength :: [Int] -> Int
myLength [] = 0
myLength (x:l) = 1 + (myLength l)

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:l) = max x (myMaximum l)

average :: [Int] -> Float
average l = fromIntegral (sum l) / fromIntegral (length l)

buildPalindrome :: [Int] -> [Int]
buildPalindrome l =  concat [(reverse l), l]

remove :: [Int] -> [Int] -> [Int]
remove l r = [ x | x <- l, notElem x r]

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten ([]:l) = flatten l
flatten ((x:l1):l2) = x:(flatten (l1:l2))

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens l = ([ x | x <- l, odd x], [x | x <- l, even x])

primeDivisors :: Int -> [Int]
primeDivisors n = [ x | x <- [2..n], ((mod n x) == 0) && isPrime x]

isPrime :: Int -> Bool
isPrime n = [ x | x <- [1..n], (mod n x) == 0 ] == [1,n]

{-
main = do
    print(myLength [4,3,1,5,4,5,2])
    print(myMaximum [4,3,1,5,4,5,2])
    print(average [1,2,3])
    print(buildPalindrome [4,3,1,5,4,5,2])
    print(remove [1,4,5,3,4,5,1,2,7,4,2] [2,4])
    print(flatten [[2,6],[8,1,4],[],[1]])
    print(oddsNevens [1,4,5,3,4,5,1,2,7,4,2])
    print(primeDivisors 255)
-}
