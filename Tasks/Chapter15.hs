-- HC9T1: Parametric Type Synonym
type Entity a = (String, a)  -- (Name, Address of type a)

main1 :: IO ()
main1 = do
    let person :: Entity String
        person = ("Alice", "123 Main St")
    print person

-- HC9T2: Parametric Data Type Box
data Box a = Empty | Has a deriving (Show)

main2 :: IO ()
main2 = do
    print (Has 42 :: Box Int)
    print (Empty :: Box Int)

-- HC9T3: Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (x + n)
addN _ Empty   = Empty

main3 :: IO ()
main3 = do
    print $ addN 5 (Has 10)
    print $ addN 5 Empty

-- HC9T4: Extract from a Box
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

main4 :: IO ()
main4 = do
    print $ extract 0 (Has 20)
    print $ extract 0 Empty

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a = Circle { color :: a, radius :: Double }
             | Rectangle { color :: a, width :: Double, height :: Double }
             deriving (Show)

main5 :: IO ()
main5 = do
    let c = Circle { color = "Red", radius = 5.0 }
        r = Rectangle { color = "Blue", width = 3.0, height = 4.0 }
    print c
    print r

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet {
    content  :: String,
    likes    :: Int,
    comments :: [Tweet]
} deriving (Show)

main6 :: IO ()
main6 = do
    let t1 = Tweet "Hello!" 10 []
        t2 = Tweet "Reply to Hello" 5 []
        t3 = Tweet "Another reply" 3 []
        mainTweet = Tweet "Main Tweet" 7 [t1, t2, t3]
    print mainTweet

-- HC9T7: Engagement Function
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

main7 :: IO ()
main7 = do
    let t1 = Tweet "Hi" 2 []
        t2 = Tweet "Yo" 3 []
        mainTweet = Tweet "Main" 5 [t1, t2]
    print $ engagement mainTweet

-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving (Show)

main8 :: IO ()
main8 = do
    let seq1 = Node 1 (Node 2 (Node 3 End))
    print seq1

-- HC9T9: Check for Element in Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y ys)
    | x == y    = True
    | otherwise = elemSeq x ys

main9 :: IO ()
main9 = do
    let seq = Node 1 (Node 2 (Node 3 End))
    print $ elemSeq 2 seq
    print $ elemSeq 5 seq

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree
           | NodeTree a (BST a) (BST a)
           deriving (Show)

main10 :: IO ()
main10 = do
    let tree = NodeTree 5
                  (NodeTree 3 EmptyTree EmptyTree)
                  (NodeTree 7 EmptyTree EmptyTree)
    print tree

-- Default entry point
main :: IO ()
main = main1  -- Change this to main2, ..., main10 to test other tasks
