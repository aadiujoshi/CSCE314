import System.Random (mkStdGen, randoms)

naturals :: [Int]
naturals = [0..]

demoNaturals = take 10 (drop 50 naturals)

randomNumbers :: [Int]
randomNumbers = randoms (mkStdGen 42)

demoSliceNaturals :: [Int]
demoSliceNaturals = take 5 (drop 20 naturals)

demoRandoms = take 10 (drop 50 randomNumbers)

primes::[Int]
primes = sieve [2..]
   where sieve (p:xs) = p:sieve[x| x <-xs, x `mod` p/= 0]

demoPrimes = take 10 (drop 200 primes)

fibs :: [Int]
fibs = 0:1:zipWith (+) fibs (tail fibs)

demoFibs = take 15 fibs

triangulars :: [Int]
triangulars = scanl1 (+) naturals

demoTriangulars = take 10 (drop 0 triangulars)

evens::[Int]
evens = [0,2..]

odds :: [Int]
odds = [1,3..]

demoSumList = take 10 (zipWith (+) evens odds)

someFibs = take 5 fibs
moreNaturals = naturals

demoZipped = zipWith (*) someFibs moreNaturals

random100 = map (`mod` 100) randomNumbers

demoZipRandomFibs = take 10 (zipWith (+) random100 fibs)

main = do
   putStrLn "Demo: Lazy Infinite Lists in Haskell"
   putStrLn "\n1. First 10 natural numbers"
   print demoNaturals

   putStrLn "\n2. 10 random numbers after dropping 5"
   print demoRandoms

   print demoPrimes

   print demoFibs

   print demoTriangulars

   print demoSumList

   print demoZipped

   putStrLn "\n3. Slice of naturals (5 numbers starting from 20)"
   print demoSliceNaturals