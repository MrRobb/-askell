
------------------------------------ BODY MASS INDEX

bmi :: Double -> Double -> Double
bmi m h = m / (h * h)

------------------------------------ INTERPRET

howfat :: Double -> Double -> String
howfat m h
    | x  < 18           = "underweight"
    | 18 <= x && x < 25 = "normal weight"
    | 25 <= x && x < 30 = "overweight"
    | 30 <= x && x < 40 = "obese"
    | 40 <= x          = "severely obese"
    where
        x = bmi m h

------------------------------------ MAIN

main :: IO ()
main = do
    line <- getLine

    if (head line) == '*' then
        return ()
    else do
        let w = words line
        let name   = w !! 0
        let weight = read (w !! 1)
        let height = read (w !! 2)
        putStr (name ++ ": " ++ (howfat weight height) ++ "\n")
        main
