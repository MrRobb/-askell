absValue :: Int -> Int
absValue x
    | x < 0 = -x
    | otherwise = x

power :: Int -> Int -> Int
power x 0 = 1
power x y = x * (power x (y - 1))

isPrime :: Int -> Bool
isPrime n = [ x | x <- [1..n], (mod n x) == 0 ] == [1,n]

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n - 2)

quickFib :: Int -> Int
quickFib n = iquickFib 0 1 n

iquickFib :: Int -> Int -> Int -> Int
iquickFib x2 x1 n
    | n /= 0 = iquickFib x1 (x2 + x1) (n - 1)
    | otherwise = x2

{-
main = do
    print(absValue (-666))
    print(power 2 3)
    print(isPrime 0)
    print(isPrime 1)
    print(slowFib 5)
    print(quickFib 40)
-}
