
------------------------------------ ROMANS RECURSIVE

c2i :: Char -> Int
c2i c
    | c == 'I' = 1
    | c == 'V' = 5
    | c == 'X' = 10
    | c == 'L' = 50
    | c == 'C' = 100
    | c == 'D' = 500
    | c == 'M' = 1000

roman2int :: String -> Int
roman2int "" = 0
roman2int [x] = c2i x
roman2int (c1:c2:l)
    | (c2i c1) < (c2i c2) = - c2i c1 + roman2int (c2:l)
    | otherwise = c2i c1 + roman2int (c2:l)

------------------------------------ ROMANS ITERATIVE

value :: Char -> Char -> Int
value x1 x2 = if (c2i x1) < (c2i x2) then - c2i x1 else c2i x1


roman2int' :: String -> Int
roman2int' "" = 0
roman2int' [x] = c2i x
roman2int' num = sum (zipWith value num ((tail num) ++ [(last num)]))

------------------------------------ ARRELS

taylor :: Float -> Float -> Float
taylor x tx = 0.5 * ( tx + (x / tx) )

arrels :: Float -> [Float]
arrels x = iterate (taylor x) x

------------------------------------ MÉS ARRELS

diff :: Float -> (Float, Float) -> Bool
diff e (x1, x2) = abs (x1 - x2) > e

arrel :: Float -> Float -> Float
arrel x e = snd (head (dropWhile (diff e) (zip (arrels x) (tail (arrels x)))))

------------------------------------ ESCRIPTURA ARBRES

data LTree a = Leaf a | Node (LTree a) (LTree a)

instance (Show a) => Show (LTree a) where
    show (Leaf x) = "{" ++ show x ++ "}"
    show (Node t1 t2) = "<" ++ (show t1) ++ "," ++ (show t2) ++ ">"

------------------------------------ BUILD

build :: [a] -> LTree a
-- build [] no es posible (la lista es no vacia)
build [x] = Leaf x
build l = Node (build l1) (build l2)
    where
        len = (div ((length l) + 1) 2)
        l1 = take len l
        l2 = drop len l

------------------------------------ ZIP TREES

zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))

zipLTrees (Node t1 t2) (Leaf y) = do Nothing
zipLTrees (Leaf x) (Node t1 t2) = do Nothing

zipLTrees (Leaf x) (Leaf y) = do
    return (Leaf (x, y))

zipLTrees (Node t1 t2) (Node t3 t4) = do
    tx <- zipLTrees t1 t3
    ty <- zipLTrees t2 t4
    return (Node tx ty)

------------------------------------ TESTS
{-
main :: IO ()
main = do
    print (roman2int "I")
    print (roman2int "IV")
    print (roman2int "MCCCXIX")
    print (roman2int "MMXVIII")
    print (roman2int' "I")
    print (roman2int' "IV")
    print (roman2int' "MCCCXIX")
    print (roman2int' "MMXVIII")
    print (take 10 $ arrels 4.0)
    print (take 10 $ arrels 100.0)
    print (arrel 4.0 0.00001)
    print (arrel 100.0 0.1)
    print (Node (Leaf 3) (Node (Leaf 8) (Leaf 7)))
    print (Node (Leaf 1) (Node (Node (Leaf 3) (Leaf 4)) (Node (Leaf 8) (Leaf 7))))
    print (Node (Leaf "Albert") (Node (Leaf "Gerard") (Leaf "Jordi")))
    print (Leaf 'x')
    print (build [3, 2, 5])
    print (build [3, 2, 8, 5, 1])
    print (build ['a', 'b', 'c', 'd'])
    print (build [[1, 2, 3]])
    let t1 = Node (Leaf "a") (Node (Leaf "b") (Leaf "c"))
    let t2 = Node (Leaf 0) (Node (Leaf 1) (Leaf 2))
    let t3 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 0)
    print (zipLTrees t1 t2)
    print (zipLTrees t1 t3)
-}
