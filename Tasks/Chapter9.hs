module Main where

-- HC9T1: Define a Parametric Type Synonym
type Entity a = (a, String)  -- Represents an entity with an address (a is the type of the entity)

-- Example usage of Entity
mainEntity :: IO ()
mainEntity = do
    let entity1 :: Entity String
        entity1 = ("Alice", "123 Wonderland St")
    let entity2 :: Entity Int
        entity2 = (1001, "456 Matrix Ave")
    print entity1  -- ("Alice", "123 Wonderland St")
    print entity2  -- (1001, "456 Matrix Ave")

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving Show

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN _ Empty = Empty
addN n (Has x) = Has (x + n)

-- Example usage of addN
mainAddN :: IO ()
mainAddN = do
    print (addN 5 (Has 10))  -- Has 15
    print (addN 3 Empty)     -- Empty

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x

-- Example usage of extract
mainExtract :: IO ()
mainExtract = do
    print (extract 0 (Has 5))  -- 5
    print (extract 0 Empty)    -- 0

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a = Circle { radius :: Float, color :: a } 
             | Rectangle { width :: Float, height :: Float, color :: a } 
             deriving Show

-- Example usage of Shape
mainShape :: IO ()
mainShape = do
    let circle = Circle { radius = 5, color = "Red" }
    let rectangle = Rectangle { width = 10, height = 5, color = "Blue" }
    print circle     -- Circle {radius = 5.0, color = "Red"}
    print rectangle  -- Rectangle {width = 10.0, height = 5.0, color = "Blue"}

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet { content :: String, likes :: Int, comments :: [Tweet] } deriving Show

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement tweet = likes tweet + sum (map engagement (comments tweet))

-- Example usage of engagement
mainEngagement :: IO ()
mainEngagement = do
    let tweet1 = Tweet { content = "First tweet", likes = 100, comments = [] }
    let tweet2 = Tweet { content = "Second tweet", likes = 50, comments = [tweet1] }
    print (engagement tweet2)  -- 150 (50 likes + 100 likes from the comment)

-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving Show

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y next) = x == y || elemSeq x next

-- Example usage of elemSeq
mainElemSeq :: IO ()
mainElemSeq = do
    let seq1 = Node 1 (Node 2 (Node 3 End))
    let seq2 = Node 1 (Node 3 (Node 5 End))
    print (elemSeq 2 seq1)  -- True
    print (elemSeq 4 seq1)  -- False

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree | NodeBST a (BST a) (BST a) deriving Show

-- Function to insert into BST
insertBST :: Ord a => a -> BST a -> BST a
insertBST x EmptyTree = NodeBST x EmptyTree EmptyTree
insertBST x (NodeBST y left right)
  | x < y     = NodeBST y (insertBST x left) right
  | x > y     = NodeBST y left (insertBST x right)
  | otherwise = NodeBST y left right  -- No duplicate values

-- Function to search in BST
searchBST :: Ord a => a -> BST a -> Bool
searchBST _ EmptyTree = False
searchBST x (NodeBST y left right)
  | x == y    = True
  | x < y     = searchBST x left
  | x > y     = searchBST x right

-- Example usage of BST
mainBST :: IO ()
mainBST = do
    let bst1 = insertBST 10 EmptyTree
    let bst2 = insertBST 5 bst1
    let bst3 = insertBST 15 bst2
    print bst3  -- NodeBST 10 (NodeBST 5 EmptyTree EmptyTree) (NodeBST 15 EmptyTree EmptyTree)
    print (searchBST 5 bst3)  -- True
    print (searchBST 20 bst3) -- False

-- Running all test cases
main :: IO ()
main = do
    putStrLn "Testing parametric type synonym Entity:"
    mainEntity
    putStrLn "\nTesting addN function for Box:"
    mainAddN
    putStrLn "\nTesting extract function for Box:"
    mainExtract
    putStrLn "\nTesting Shape with record syntax:"
    mainShape
    putStrLn "\nTesting engagement function for tweets:"
    mainEngagement
    putStrLn "\nTesting elemSeq function for Sequence:"
    mainElemSeq
    putStrLn
