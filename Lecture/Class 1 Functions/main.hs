import Data.Text.Lazy.Read (double)
{-  Aadi Joshi
    09/05/2025
-}

{- Write the functions sumList
                       doubleSum
                       sumEvenNumbers
                       SumDoubleEvenNumbers
Then, use ghci to test each function
When ready, upload your code and paste your 
output to the questions below. -}

sumList :: [Int] -> Int
sumList [] = 0
sumList (x: xs) = x + sumList xs

doubleSum :: [Int] -> Int
doubleSum [] = 0
doubleSum (x: xs) = 2*x + doubleSum xs

sumEvenNumbers :: [Int] -> Int
sumEvenNumbers list = sumList (filter even list)

sumDoubleEvenNumbers :: [Int] -> Int
sumDoubleEvenNumbers list = doubleSum (filter even list)

main :: IO ()
main = do
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  print (sumList numbers)               -- Output: 55
  print (doubleSum numbers)             -- Output: 110
  print (sumEvenNumbers numbers)        -- Output: 30
  print (sumDoubleEvenNumbers numbers)  -- Output: 60

