------------------------------------ LLISTA INFINITA

multEq :: Int -> Int -> [Int]
multEq x y = iterate (x * y *) 1

------------------------------------ SELECCIO

pos :: (Eq a) => [a] -> a -> Int
pos [] x = -1
pos (y:l) x
    | x == y = 0
    | nextPos == -1 = -1
    | otherwise = 1 + nextPos
    where
        nextPos = pos l x

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst l1 l2 l3 = [ x | x <- l1, (pos l2 x) >= 0 , (pos l3 x) < 0 || (pos l2 x) < (pos l3 x) ]

------------------------------------ ITERATE AMB SCANL

myIterate :: (a -> a) -> a -> [a]
myIterate fn x = scanl (\x1 x2 -> fn x1) x (myIterate fn x)

------------------------------------ TAULA DE SIMBOLS

type SymTab a = String -> Maybe a

empty :: SymTab a
empty = (\x -> Nothing)

get :: SymTab a -> String -> Maybe a
get t s = (t s)

set :: SymTab a -> String -> a -> SymTab a
set t k v x
    | k == x = return v
    | otherwise = get t x

------------------------------------ EXPRESSIONS AMB SIMBOLS

data Expr a
    = Val a
    | Var String
    | Sum (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving Show

eval :: (Num a) => SymTab a -> Expr a -> Maybe a
eval t (Val a) = return a
eval t (Var s) = get t s
eval t (Sum e1 e2) = do
    e1x <- eval t e1
    e2x <- eval t e2
    return (e1x + e2x)
eval t (Sub e1 e2) = do
    e1x <- eval t e1
    e2x <- eval t e2
    return (e1x - e2x)
eval t (Mul e1 e2) = do
    e1x <- eval t e1
    e2x <- eval t e2
    return (e1x * e2x)

------------------------------------ TESTS

{-
main = do
    print(take 6 $ multEq 2 3)
    print(take 5 $ multEq 3 7)

    print(selectFirst [] [] [])
    print(selectFirst [8,4,5,6,12,1] [] [8,6,5,4,1])
    print(selectFirst [8,4,5,6,12,1] [4,5,6,2,8,12] [])
    print(selectFirst [8,4,5,6,12,1] [4,5,6,2,8,12] [8,6,5,4,1])

    print(take 10 $ myIterate (+1) 0)
    print(take 10 $ myIterate (*2) 1)
    print(take 10 $ myIterate ('a':) [])
    print(take 8 $ myIterate (++"y") "x")

    print(get (set empty "a" 1) "a")
    print(get (set empty "a" 1) "b")
    print(get (set (set empty "a" 1) "b" 2) "a")
    print(get (set (set empty "a" 1) "b" 2) "b")
    print(get (set (set empty "a" 1) "b" 2) "c")
    print(get (set (set empty "a" 1) "a" 2) "a")

    let st1 = set (set empty "a" 1) "b" 2
    let st2 = set (set empty "a" 4) "b" 3
    let e1 = Mul (Val 5) (Sum (Var "a") (Var "b"))
    let e2 = Mul (Val 5) (Sum (Var "a") (Var "c"))
    let e3 = Sub (Var "a") (Var "b")
    print(eval st1 e1)
    print(eval st2 e1)
    print(eval st1 e2)
    print(eval st2 e2)
    print(eval st1 e3)
    print(eval st2 e3)
-}
