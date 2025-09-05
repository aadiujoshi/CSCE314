double :: Int -> Int
double n = n + n


quadruple :: Int -> Int
quadruple n = double n + double n


square :: Int -> Int
square n = n * n


getC :: (Double, Double) -> Double
getC (a, b) = sqrt(a^2 + b**2)


inRange :: Int -> Int -> Int -> Bool
inRange min max x = x >= min && x <= max


sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

inverseSqrt :: Double -> Double
inverseSqrt n = 1 / sqrt n

main = do
    putStrLn "The double of 22 is:"
    print (double 22)
    putStrLn "The square of 55 is:"
    print (square 55)
    putStrLn "The inverse square root of 120 is: "
    print (inverseSqrt 120) 