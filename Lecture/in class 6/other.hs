import System.Random (mkStdGen, randoms)

naturals :: [Integer]
naturals = [1..]

primes :: [Integer]
primes = sieve [2..]
 where
   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

triangulars :: [Integer]
triangulars = scanl1 (+) naturals

evens :: [Integer]
evens = [0,2..]

odds :: [Integer]
odds = [1,3..]

printPrimes :: [Integer]
printPrimes = take 11 (drop 999 primes)

fibonacci :: [Integer]
fibonacci = take 10 (drop 199 fibs)

trigs :: [Integer]
trigs = take 10 (drop 499 triangulars)

primesMod4 :: [Integer]
primesMod4 = filter (\p -> p `mod` 4 == 1) primes

printPrime4 :: [Integer]
printPrime4 = take 10 (drop 999 primesMod4)

someFunc :: [Integer]
someFunc = zipWith (+) fibs triangulars

printSomeFunc :: [Integer]
printSomeFunc = take 10 (drop 100 someFunc)

randNum :: [Int]
randNum = randoms (mkStdGen 31415) :: [Int]

noisyFibonacci :: [Integer]
noisyFibonacci = zipWith (\f r -> f + fromIntegral (r `mod` 100)) fibs randNum

printNoisyFibonacci :: [Integer]
printNoisyFibonacci = take 10 (drop 50 noisyFibonacci)

demoMaxEvensOdds :: [Integer]
demoMaxEvensOdds = take 10 (drop 100 (zipWith max evens odds))

deck :: [Int]
deck = [1..52]

shuffle :: [a] -> [a]
shuffle xs = evens ++ odds
  where
    evens = [x | (x,i) <- zip xs [0..], even i]
    odds  = [x | (x,i) <- zip xs [0..], odd i]

dealHand :: [Int]
dealHand = take 13 (shuffle deck)

main :: IO ()
main = do
   print printPrimes
   print fibonacci
   print trigs
   print printPrime4
   print dealHand