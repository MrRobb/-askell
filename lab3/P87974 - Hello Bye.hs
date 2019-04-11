
------------------------------------ SAY HI

sayHi :: Char -> IO ()
sayHi c
    | c == 'A' || c == 'a'  = putStrLn "Hello!"
    | otherwise = putStrLn "Bye!"

------------------------------------ MAIN

main :: IO ()
main = do
    line <- getLine
    sayHi (head line)
