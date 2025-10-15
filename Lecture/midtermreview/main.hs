-- data Maybe a = Just a | Nothing

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

countLongerThan :: Int -> [String] -> Int
-- countLongerThan _ [] = 0
-- countLongerThan count (cur:rest)
--     | (length cur) > count = 1 + countLongerThan count rest
--     | otherwise = countLongerThan count rest 

-- fdas :: Maybe a -> Int
-- fdas (Maybe x) = Just

countLongerThan len list = foldl (\value element -> if (length element > len) then value + 1 else value) 0 list

startsWith :: Char -> [String] -> [String]
-- startsWith _ [] = []
-- startsWith c ((x:xs):rest)
--     | x == c = [x:xs] ++ startsWith c rest
--     | otherwise = startsWith c rest

startsWith c list = filter (\(x:xs) -> x == c) list

totalChars :: [String] -> Int
totalChars list = length (foldl (++) "" list)

analyzeWords :: [String] -> (Int, [String], Int)
analyzeWords list = (countLongerThan 3 list, startsWith 'h' list, totalChars list)

main :: IO()
main = do
    print (countLongerThan 3 ["hi", "world", "cat", "Haskell", "Haskell", "Haskell", "Haskell"])