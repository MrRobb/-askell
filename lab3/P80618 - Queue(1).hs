
------------------------------------ QUEUE

data Queue a = Queue [a] [a]
     deriving (Show)

------------------------------------ CREATE

create :: Queue a
create = Queue [] []

------------------------------------ PUSH

push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = (Queue l1 (x:l2))

------------------------------------ POP

pop :: Queue a -> Queue a
pop (Queue [] l2) = (Queue (tail (reverse l2)) [])
pop (Queue (x:l1) l2) = (Queue l1 l2)

------------------------------------ TOP

top :: Queue a -> a
top (Queue [] l2) = last l2
top (Queue (x:l1) l2) = x

------------------------------------ EMPTY

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False

------------------------------------ EQUALITY

instance Eq a => Eq (Queue a)
    where
        (Queue l1 l2) == (Queue l3 l4) = l1 ++ (reverse l2) == l3 ++ (reverse l4)

------------------------------------ TESTS
{-
main = do
    let c = push 3 (push 2 (push 1 create))
    print(c)
    print(top c)
    print(pop c)
    print(empty $ pop c)
    print(empty $ pop $ pop $ c)
    print(empty $ pop $ pop $ pop c)
    let c1 = push 4 (pop (push 3 (push 2 (push 1 create))))
    let c2 = push 4 (push 3 (push 2 create))
    print(c1)
    print(c2)
    print(c1 == c2)
-}
