--Add your name here: Aadi Joshi

-- Step 1: Generate an Arithmetic Sequence (generateSequence)
-- look at the code in main to determine the signature, 
-- then the code to create the sequence

generateSequence :: Int -> Int -> Int -> [Int]
generateSequence start step end = [start, (start + step)..end]


-- Step 2: Filter Values Based on a Condition
-- return a list of numbers that are evenly divided by your divisor
filterDivisible :: Int -> [Int] -> [Int]
filterDivisible divisor sequence = filter (\x -> mod x divisor == 0) sequence

-- Step 3: Transform the Values
-- create a function to square a value, 
squareValue :: Int -> Int
squareValue x = x * x

-- then create a function to apply
-- the function to the sequence
transformSequence :: [Int] -> [Int]
transformSequence sequence = map squareValue sequence


-- Step 4: Calculate the Sum 
sumSequence :: [Int] -> Int
sumSequence sequence = foldl (\p n -> p + n) 0 sequence

-- Main Program
main :: IO ()
main = do
  putStrLn "Enter the start of the sequence:"
  start <- readLn
  putStrLn "Enter the step size:"
  step <- readLn
  putStrLn "Enter the end of the sequence to generate:"
  n <- readLn

--Uncomment out these lines as you get to each function.

  putStrLn "Enter the divisor for filtering:"
  divisor <- readLn

   -- Generate the sequence
  let sequence = generateSequence start step n
  putStrLn $ "Generated sequence: " ++ show sequence

  let filtered = filterDivisible divisor sequence
  putStrLn $ "Filtered sequence (divisible by " ++ show divisor ++ "): " ++ show filtered

  let transformed = transformSequence filtered
  putStrLn $ "Transformed sequence (squared values): " ++ show transformed

  let sumTransformed = sumSequence transformed
  putStrLn $ "Sum of transformed sequence: " ++ show sumTransformed