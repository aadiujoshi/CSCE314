import Data.Char (isDigit, isSpace)

-- 1) Polymorphism: keep only successful results
mapOptional :: (a -> Maybe b) -> [a] -> [b]
mapOptional _ [] = []
mapOptional mapper (x:xs) = 
    case mapper x of
        Nothing -> mapOptional mapper xs
        Just y -> y : mapOptional mapper xs

-- 2) Safe integer parser for a single token using reads
readIntMaybe :: String -> Maybe Int
readIntMaybe s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

-- 3) Replace non-digit/non-sign with spaces to separate tokens
-- Keep '-' so negative numbers remain intact.
normalizeDigits :: String -> String
normalizeDigits = map (\c -> if isDigit c || c == '-' then c else ' ')

-- 4) Extract integers from messy text
extractInts :: String -> [Int]
extractInts = mapOptional readIntMaybe . words . normalizeDigits

-- 5) Variants with Maybe and Either
extractIntsMaybe :: String -> Maybe [Int]
extractIntsMaybe s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

extractIntsEither :: String -> Either String [Int]
extractIntsEither s = case reads s of
    [(n, "")] -> Right [n]
    _ -> Left "no integers found"