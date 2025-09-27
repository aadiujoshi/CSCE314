import Data.List (foldl')


sumList:: [Int] -> Int
sumList (x:xs) = x + sumList xs


productList :: [Int] -> Int
productList (x:xs) = x * productList xs


len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs


rev :: [a] -> [a]
rev [] = []
rev(x:xs) = rev xs ++ [x]


isPrime :: Int -> Bool
isPrime n = n > 1 && all ((/=0) . mod n) [2..floor (sqrt (fromIntegral n))]


sumListR :: [Int] -> Int
sumListR = foldr (+) 0


lenR:: [a] -> Int
lenR = foldr (\_ acc -> 1 + acc) 0


revR :: [a] -> [a]
revR = foldr (\x acc -> acc ++ [x]) []


revL :: [a] -> [a]
revL = foldl' (\acc x -> x : acc) []


add :: Int -> Int -> Int
add x y = x + y
