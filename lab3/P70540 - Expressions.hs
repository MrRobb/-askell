
------------------------------------ EXPR

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

------------------------------------ EVAL1

eval1 :: Expr -> Int
eval1 (Val v) = v
eval1 (Add e1 e2) = (eval1 e1) + (eval1 e2)
eval1 (Sub e1 e2) = (eval1 e1) - (eval1 e2)
eval1 (Mul e1 e2) = (eval1 e1) * (eval1 e2)
eval1 (Div e1 e2) = div (eval1 e1) (eval1 e2)

------------------------------------ EVAL2

eval2 :: Expr -> Maybe Int
eval2 (Val v) = Just v

eval2 (Add e1 e2) = do
    x1 <- (eval2 e1)
    x2 <- (eval2 e2)
    return (x1 + x2)

eval2 (Sub e1 e2) = do
    x1 <- (eval2 e1)
    x2 <- (eval2 e2)
    return (x1 - x2)

eval2 (Mul e1 e2) = do
    x1 <- (eval2 e1)
    x2 <- (eval2 e2)
    return (x1 * x2)

eval2 (Div e1 e2) = do
    x1 <- (eval2 e1)
    x2 <- (eval2 e2)
    if x2 == 0 then Nothing
    else return (div x1 x2)

------------------------------------ EVAL3

eval3 :: Expr -> Either String Int

eval3 (Val v) = return v

eval3 (Add e1 e2) = do
    x1 <- (eval3 e1)
    x2 <- (eval3 e2)
    return (x1 + x2)

eval3 (Sub e1 e2) = do
    x1 <- (eval3 e1)
    x2 <- (eval3 e2)
    return (x1 - x2)

eval3 (Mul e1 e2) = do
    x1 <- (eval3 e1)
    x2 <- (eval3 e2)
    return (x1 * x2)

eval3 (Div e1 e2) = do
    x1 <- (eval3 e1)
    x2 <- (eval3 e2)
    if x2 == 0 then (Left "div0")
    else return (div x1 x2)

------------------------------------ TESTS

{-
main = do
    print(eval1 (Val 2))
    print(eval1 (Add (Val 2) (Val 3)))
    print(eval1 (Sub (Val 2) (Val 3)))
    print(eval1 (Div (Val 4) (Val 2)))
    print(eval1 (Mul (Add (Val 2) (Val 3)) (Sub (Val 2) (Val 3))))
    print(eval2 (Val 2))
    print(eval2 (Add (Val 2) (Val 3)))
    print(eval2 (Sub (Val 2) (Val 3)))
    print(eval2 (Div (Val 4) (Val 2)))
    print(eval2 (Mul (Add (Val 2) (Val 3)) (Sub (Val 2) (Val 3))))
    print(eval2 (Div (Val 4) (Val 0)))
    print(eval2 (Add (Div (Val 4) (Val 0)) (Val 3)))
    print(eval2 (Add (Val 3) (Div (Val 4) (Val 0))))
    print(eval3 (Val 2))
    print(eval3 (Add (Val 2) (Val 3)))
    print(eval3 (Sub (Val 2) (Val 3)))
    print(eval3 (Div (Val 4) (Val 2)))
    print(eval3 (Mul (Add (Val 2) (Val 3)) (Sub (Val 2) (Val 3))))
    print(eval3 (Div (Val 4) (Val 0)))
    print(eval3 (Add (Div (Val 4) (Val 0)) (Val 3)))
    print(eval3 (Add (Val 3) (Div (Val 4) (Val 0))))
-}
