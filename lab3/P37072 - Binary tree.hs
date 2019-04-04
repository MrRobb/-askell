
------------------------------------ BINARY TREE

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

------------------------------------ TREE SIZE

size :: Tree a -> Int
size Empty = 0
size (Node root t1 t2) = 1 + (size t1) + (size t2)

------------------------------------ HEIGHT

height :: Tree a -> Int
height Empty = 0
height (Node root t1 t2) = 1 + (max (height t1) (height t2))

------------------------------------ EQUAL

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node root t1 t2) Empty = False
equal Empty (Node root t1 t2) = False
equal (Node root1 t11 t12) (Node root2 t21 t22) =
    (root1 == root2) && (equal t11 t21) && (equal t12 t22)

------------------------------------ ISOMORPHIC

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic (Node root t1 t2) Empty = False
isomorphic Empty (Node root t1 t2) = False
isomorphic (Node root1 a1 a2) (Node root2 b1 b2) =
    root1 == root2 &&
    ((isomorphic a1 b1 && isomorphic a2 b2) ||
    (isomorphic a1 b2 && isomorphic a2 b1))

------------------------------------ PREORDER

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node root t1 t2) = root : (preOrder t1) ++ (preOrder t2)

------------------------------------ POSTORDER

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node root t1 t2) = (postOrder t1) ++ (postOrder t2) ++ [root]

------------------------------------ INORDER

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node root t1 t2) = (inOrder t1) ++ root : (inOrder t2)

------------------------------------ BREADTH FIRST

bfs :: [Tree a] -> [a]
bfs [] = []
bfs ((Empty):l) = (bfs l)
bfs ((Node root t1 t2):l) = root : (bfs (l ++ [t1, t2]))

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]

------------------------------------ BUILD

build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:lp) li = (Node x (build lpl lil) (build lpr lir))
    where
        (lil, _:lir) = span (/= x) li
        (lpl, lpr) = splitAt (length lil) lp



------------------------------------ OVERLAP

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap fn t Empty = t
overlap fn Empty t = t
overlap fn (Node root1 a1 a2) (Node root2 b1 b2) = (Node (fn root1 root2) t1 t2)
    where
        t1 = overlap fn a1 b1
        t2 = overlap fn a2 b2

------------------------------------ TESTS

{-
main = do
    let t7 = Node 7 Empty Empty
    let t6 = Node 6 Empty Empty
    let t5 = Node 5 Empty Empty
    let t4 = Node 4 Empty Empty
    let t3 = Node 3 t6 t7
    let t2 = Node 2 t4 t5
    let t1 = Node 1 t2 t3
    let t1' = Node 1 t3 t2
    print(size t1)
    print(height t1)
    print(equal t2 t3)
    print(isomorphic t1 t1')
    print(preOrder t1)
    print(postOrder t1)
    print(inOrder t1)
    print(breadthFirst t1)
    print(build [1,2,4,5,3] [4,2,5,1,3])
    print(overlap (+) t2 t3)
    print(overlap (+) t1 t3)
-}
