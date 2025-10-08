module Week7Code_Along where
-- Week 7 Code-Along (CSCE 314)
-- Topic: Trees in FP -> Haskell (binary trees, folds, traversals, Functor/Foldable)
-- Load in GHCi: :load Week7Code_AlongV2.hs
import Data.Monoid (Sum(..))
--------------------------------------------------------------------------------
-- 1) A simple binary tree (values at leaves; nodes only connect subtrees)
--------------------------------------------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)
-- Map over trees (alias to fmap; we also supply a Functor instance below)
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- Fold over trees (catamorphism for our leaf-labeled tree)
-- leafCase handles a leaf's value; nodeCase combines the two subtree results.

foldTree :: (a -> r) -> (r -> r -> r) -> Tree a -> r
foldTree leafCase _ (Leaf a) = leafCase a
foldTree leafCase nodeCase (Node l r) = nodeCase (foldTree leafCase nodeCase l) (foldTree leafCase nodeCase r)

-- Standard computations via fold
numLeaves :: Tree a -> Int
numLeaves = foldTree (const 1) (+)
height :: Tree a -> Int
height = foldTree (const 1) (\l r -> 1 + max l r)
leaves :: Tree a -> [a]
leaves = foldTree (:[]) (++)

-- Build a (roughly) balanced tree from a non-empty list by splitting halves.
fromListBalanced :: [a] -> Tree a
fromListBalanced [] = error "fromListBalanced: empty list"
fromListBalanced [x] = Leaf x
fromListBalanced xs = 
    let n = length xs 
        mid = n `div` 2 
        (ls, rs) = splitAt mid xs
    in Node (fromListBalanced ls) (fromListBalanced rs)

-- Instances: Functor (map) and Foldable (generic reductions like sum, toList)
instance Functor Tree where
    fmap = mapTree
instance Foldable Tree where
    foldMap f (Leaf a) = f a
    foldMap f (Node l r) = foldMap f l <> foldMap f r

-- Inorder traversal (leaf order) equals 'toList' from Foldable
inorder :: Tree a -> [a]
inorder = leaves

-- Make a small balanced tree for demos
t7 :: Tree Int
t7 = fromListBalanced [1..7]

t20 :: Tree Int
t20 = fromListBalanced [1..20]

evenTree :: Tree Int
evenTree = fromListBalanced [0,2..50]

squareTree :: Tree Int
squareTree = fromListBalanced (map (\x -> x * x) [1..20])

-- Examples demonstrating Foldable
sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMap Sum
productTree :: Num a => Tree a -> a
productTree = foldr (*) 1

-- A few quick labeled demos for 'main' (for convenience)
demo :: IO ()
demo = do
    putStrLn "== Binary Tree (values at leaves) =="
    putStrLn $ "t7 = fromListBalanced [1..7] -> " ++ show t7
    putStrLn $ "numLeaves t7 = " ++ show (numLeaves t7)
    putStrLn $ "height t7 = " ++ show (height t7)
    putStrLn $ "inorder t7 = " ++ show (inorder t7)
    putStrLn $ "fmap (*2) t7 = " ++ show (fmap (*2) t7)
    putStrLn $ "sumTree t7 = " ++ show (sumTree t7)
    main :: IO ()

main = demo