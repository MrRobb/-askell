
------------------------------------ ONES

ones :: [Integer]
ones = repeat 1

------------------------------------ NATS

nats :: [Integer]
nats = iterate (+1) 0

------------------------------------ INTS

ints :: [Integer]
ints = 0 : concatMap (\x -> [x, -x]) (tail nats)

------------------------------------ TRIANGULARS

triangulars :: [Integer]
triangulars = map (\x -> x * (x + 1) `div` 2) nats

------------------------------------ FACTORIALS

factorials :: [Integer]
factorials = scanl (*) 1 (tail nats)

------------------------------------ FIBS

fibs :: [Integer]
fibs = 0 : 1 : 1 : zipWith (+) (tail (tail fibs)) (tail fibs)

------------------------------------ PRIMES

factors :: Integer -> [Integer]
factors n = [ x | x <- [1..n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = factors n == [1, n]

primes :: [Integer]
primes = filter isPrime nats

------------------------------------ HAMMINGS

mult2 :: [Integer] -> [Integer]
mult2 = map (*2)

mult3 :: [Integer] -> [Integer]
mult3 = map (*3)

mult5 :: [Integer] -> [Integer]
mult5 = map (*5)

merge :: [Integer] -> [Integer] -> [Integer]
merge [] l = l
merge l [] = l
merge (x1:l1) (x2:l2)
    | x1 == x2  = x1 : (merge l1 l2)
    | x1 < x2   = x1 : (merge l1 (x2:l2))
    | otherwise = x2 : (merge (x1:l1) l2)

hammings :: [Integer]
hammings = 1 : merge (merge (mult2 hammings) (mult3 hammings)) (mult5 hammings)

------------------------------------ LOOK AND SAY

look :: [Char] -> Integer
look [] = 0
look [_] = 1
look (c1:c2:s)
    | c1 == c2 = 1 + (look (c2:s))
    | otherwise = 1

say :: [Char] -> [Char]
say [] = []
say s = (show count) ++ (head s) : (say (drop (fromIntegral count) s)) where count = look s

lookNsay :: [Integer]
lookNsay = iterate (read . say . show) 1

------------------------------------ TARTAGLIA

pascalTriangle :: [Integer] -> [Integer]
pascalTriangle [] = []
pascalTriangle [_] = []
pascalTriangle (x:y:l) = (x + y):(pascalTriangle (y:l))

tartaglia :: [[Integer]]
tartaglia = iterate (\l -> 1 : (pascalTriangle l) ++ [1]) [1]

------------------------------------ TESTS

{-
main = do
    print(take 8 ones)
    print(take 8 nats)
    print(take 8 ints)
    print(take 8 triangulars)
    print(take 8 factorials)
    print(take 8 fibs)
    print(take 8 primes)
    print(take 200 hammings)
    print(take 8 lookNsay)
    print(look (show 11))
    print(say (show 11))
    print(take 6 tartaglia)
-}
