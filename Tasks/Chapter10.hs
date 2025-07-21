module Main where

-- HC10T1: ShowSimple Type Class
class ShowSimple a where
    showSimple :: a -> String

-- PaymentMethod Type for HC10T1
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

instance ShowSimple PaymentMethod where
    showSimple Cash = "Cash Payment"
    showSimple Card = "Card Payment"
    showSimple Cryptocurrency = "Cryptocurrency Payment"

-- HC10T2: Summable Type Class
class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = sum

-- HC10T3: Comparable Type Class
data Blockchain = Block String Int deriving Show

class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
    compareWith (Block _ height1) (Block _ height2) = compare height1 height2

-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving (Show)

instance Eq a => Eq (Box a) where
    Empty == Empty = True
    Has x == Has y = x == y
    _ == _ = False

-- HC10T5: ShowDetailed Type Class
class ShowDetailed a where
    showDetailed :: a -> String

data User = User { username :: String, age :: Int } deriving Show

instance ShowDetailed User where
    showDetailed (User name age) = "User: " ++ name ++ ", Age: " ++ show age

-- HC10T6: Mutual Recursion in Eq for Blockchain
instance Eq Blockchain where
    (Block id1 _) == (Block id2 _) = id1 == id2

-- HC10T7: Convertible Type Class
class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert Cash = "Cash"
    convert Card = "Card"
    convert Cryptocurrency = "Cryptocurrency"

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

instance AdvancedEq Blockchain where
    compareEquality (Block id1 _) (Block id2 _) = id1 == id2

-- HC10T9: MinMax Type Class
class MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound
    maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable [Char] where
    concatWith = (++)

-- Testing all the functions

main :: IO ()
main = do
    -- HC10T1: ShowSimple for PaymentMethod
    putStrLn $ "ShowSimple Cash: " ++ showSimple Cash
    putStrLn $ "ShowSimple Card: " ++ showSimple Card
    putStrLn $ "ShowSimple Cryptocurrency: " ++ showSimple Cryptocurrency

    -- HC10T2: Summable for Int
    let intList = [1, 2, 3, 4, 5]
    putStrLn $ "Sum of integers: " ++ show (sumUp intList)

    -- HC10T3: Comparable for Blockchain
    let block1 = Block "Block1" 1
    let block2 = Block "Block2" 2
    putStrLn $ "Comparison of blocks: " ++ show (compareWith block1 block2)

    -- HC10T4: Eq instance for Box
    let box1 = Has 10
    let box2 = Has 10
    let box3 = Has 20
    let emptyBox = Empty
    putStrLn $ "Box1 == Box2: " ++ show (box1 == box2)
    putStrLn $ "Box1 == Box3: " ++ show (box1 == box3)
    putStrLn $ "Box1 == Empty: " ++ show (box1 == emptyBox)

    -- HC10T5: ShowDetailed for User
    let user1 = User "Alice" 30
    let user2 = User "Bob" 25
    putStrLn $ "User1 Details: " ++ showDetailed user1
    putStrLn $ "User2 Details: " ++ showDetailed user2

    -- HC10T6: Mutual Recursion in Eq for Blockchain
    let block3 = Block "Block3" 3
    let block4 = Block "Block3" 3
    putStrLn $ "Block3 == Block4: " ++ show (block3 == block4)

    -- HC10T7: Convertible for PaymentMethod to String
    putStrLn $ "Convert Cash: " ++ convert Cash
    putStrLn $ "Convert Card: " ++ convert Card
    putStrLn $ "Convert Cryptocurrency: " ++ convert Cryptocurrency

    -- HC10T8: AdvancedEq for Blockchain
    putStrLn $ "Block3 == Block4 using compareEquality: " ++ show (compareEquality block3 block4)

    -- HC10T9: MinMax for Int
    putStrLn $ "Min Value: " ++ show (minValue :: Int)
    putStrLn $ "Max Value: " ++ show (maxValue :: Int)

    -- HC10T10: Concatenatable for String
    let str1 = "Hello, "
    let str2 = "world!"
    putStrLn $ "Concatenated string: " ++ concatWith str1 str2
