------------------------------------------------------------
-- HC17T1: Severity and Semigroup instance
------------------------------------------------------------

data Severity = Low | Medium | High | Critical
    deriving (Eq, Show, Ord, Enum)

instance Semigroup Severity where
    (<>) = max

main1 :: IO ()
main1 = do
    print $ Medium <> High     -- should print High
    print $ Low <> Critical    -- should print Critical

------------------------------------------------------------
-- HC17T2: Min and Max Newtypes with Semigroup
------------------------------------------------------------

newtype MinVal a = MinVal a deriving (Show)
newtype MaxVal a = MaxVal a deriving (Show)

instance Ord a => Semigroup (MinVal a) where
    MinVal x <> MinVal y = MinVal (min x y)

instance Ord a => Semigroup (MaxVal a) where
    MaxVal x <> MaxVal y = MaxVal (max x y)

main2 :: IO ()
main2 = do
    print $ MinVal 5 <> MinVal 3  -- MinVal 3
    print $ MaxVal 2 <> MaxVal 7  -- MaxVal 7

------------------------------------------------------------
-- HC17T3: Monoid instance for Severity
------------------------------------------------------------

instance Monoid Severity where
    mempty = Low

main3 :: IO ()
main3 = do
    print $ mconcat [Low, Medium, Critical, High]  -- Critical

------------------------------------------------------------
-- HC17T4: Monoid instance for Sum
------------------------------------------------------------

-- Already defined in Data.Monoid:
-- instance Num a => Monoid (Sum a) where
--     mempty = Sum 0

main4 :: IO ()
main4 = do
    print $ mconcat [Sum 1, Sum 2, Sum 3]  -- Sum {getSum = 6}

------------------------------------------------------------
-- HC17T5: combineLists Function
------------------------------------------------------------

combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

main5 :: IO ()
main5 = do
    print $ combineLists [1,2] [3,4]  -- [1,2,3,4]

------------------------------------------------------------
-- HC17T6: maxSeverity Function
------------------------------------------------------------

maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

main6 :: IO ()
main6 = do
    print $ maxSeverity [Low, High, Medium]  -- High

------------------------------------------------------------
-- HC17T7: multiplyProducts Function
------------------------------------------------------------

multiplyProducts :: [Product Int] -> Product Int
multiplyProducts = mconcat

main7 :: IO ()
main7 = do
    print $ multiplyProducts [Product 2, Product 3, Product 4]  -- Product 24

------------------------------------------------------------
-- HC17T8: foldWithSemigroup Function
------------------------------------------------------------

foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

main8 :: IO ()
main8 = do
    print $ foldWithSemigroup ["Hello", " ", "World!"]  -- "Hello World!"
    print $ foldWithSemigroup [Sum 1, Sum 2, Sum 3]     -- Sum 6

------------------------------------------------------------
-- HC17T9: Config Data Type and Semigroup Instance
------------------------------------------------------------

data Config = Config
    { loggingLevel :: Severity
    , timeout      :: Int
    , retries      :: Int
    } deriving (Show)

instance Semigroup Config where
    (<>) (Config l1 t1 r1) (Config l2 t2 r2) =
        Config (max l1 l2) (min t1 t2) (max r1 r2)

main9 :: IO ()
main9 = do
    let c1 = Config Medium 30 3
    let c2 = Config High 20 5
    print $ c1 <> c2
    -- Expected: loggingLevel High, timeout 20, retries 5

------------------------------------------------------------
-- HC17T10: Monoid Instance for Config
------------------------------------------------------------

instance Monoid Config where
    mempty = Config Low maxBound 0

main10 :: IO ()
main10 = do
    let c1 = Config Medium 30 3
    print $ c1 <> mempty  -- should return c1
    print $ mempty <> c1  -- should return c1

------------------------------------------------------------
-- Select the main to run
------------------------------------------------------------

main :: IO ()
main = main1  -- Change to main2 .. main10 to test each task
