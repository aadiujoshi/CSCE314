main :: IO ()

data Node t = Node t (Node t) | Empty deriving(Eq, Show)
data TreeNode t = TreeNode t (TreeNode t) (TreeNode t) | Empty_ deriving(Eq, Show)

sampleList :: Node Int
sampleList = Node 2 (Node 1 Empty)

buildList :: Int -> Int -> Node Int
buildList start end = buildNodeListRecurs [start..end]

buildNodeListRecurs :: [Int] -> Node Int
buildNodeListRecurs [] = Empty
buildNodeListRecurs (x:xs) = Node x (buildNodeListRecurs xs)

buildNodeListFold :: [Int] -> Node Int
buildNodeListFold [] = Empty
buildNodeListFold list = foldr (\val prevNode -> Node val prevNode) Empty list

--end is exclusive
sublist :: [Int] -> Int -> Int -> [Int]
sublist list start end = take (end - start) (drop start list)

nodeListToStringRecurs :: Show t => Node t -> String
nodeListToStringRecurs Empty = "End"
nodeListToStringRecurs (Node t next) = show t ++ " -> " ++nodeListToStringRecurs next

main = do
    putStrLn "===== Node List Tests ====="

    -- Test 1: buildNodeListRecurs
    let recList = buildNodeListRecurs [1,2,3]
    putStrLn $ "Recursive build: " ++ nodeListToStringRecurs recList
    print $ recList == Node 1 (Node 2 (Node 3 Empty))  -- should be True

    -- Test 2: buildNodeListFold
    let foldList = buildNodeListFold [1,2,3]
    putStrLn $ "Fold build: " ++ nodeListToStringRecurs foldList
    print $ foldList == Node 1 (Node 2 (Node 3 Empty)) -- should be True

    -- Test 3: sublist
    let lst = [10,20,30,40,50]
    let sub = sublist lst 1 4
    putStrLn $ "Sublist 1..4 of [10,20,30,40,50]: " ++ show sub
    print $ sub == [20,30,40]  -- should be True

    -- Test 4: nodeListToStringRecurs
    let stringRep = nodeListToStringRecurs (Node 5 (Node 6 Empty))
    putStrLn $ "String representation of Node 5 -> 6: " ++ stringRep
    print $ stringRep == "5 -> 6 -> End"

    putStrLn "\n===== TreeNode Tests ====="

    -- Simple binary tree
    let tree = TreeNode 1 (TreeNode 2 Empty_ Empty_) (TreeNode 3 Empty_ Empty_)
    print tree
    print $ case tree of
        TreeNode v l r -> v == 1
        Empty_         -> False