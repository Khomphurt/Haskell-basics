------------------------------------------------------------
-- HC18T1: mapToLower using fmap
------------------------------------------------------------

mapToLower :: String -> String
mapToLower = fmap toLower

main1 :: IO ()
main1 = do
    print $ mapToLower "HELLO World"  -- "hello world"

------------------------------------------------------------
-- HC18T2: Functor instance for Tree
------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Functor)

main2 :: IO ()
main2 = do
    let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    print tree

------------------------------------------------------------
-- HC18T3: incrementTreeValues using Functor
------------------------------------------------------------

incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

main3 :: IO ()
main3 = do
    let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    print $ incrementTreeValues tree

------------------------------------------------------------
-- HC18T4: mapToBits using fmap
------------------------------------------------------------

mapToBits :: [Bool] -> String
mapToBits = fmap (\b -> if b then '1' else '0')

main4 :: IO ()
main4 = do
    print $ mapToBits [True, False, True, True]  -- "1011"

------------------------------------------------------------
-- HC18T5: Functor instance for Either
------------------------------------------------------------

-- Already defined in Prelude:
-- instance Functor (Either e) where
--     fmap _ (Left e)  = Left e
--     fmap f (Right x) = Right (f x)

main5 :: IO ()
main5 = do
    print $ fmap (+1) (Right 4 :: Either String Int)  -- Right 5
    print $ fmap (+1) (Left "Error" :: Either String Int) -- Left "Error"

------------------------------------------------------------
-- HC18T6: applyToMaybe using fmap
------------------------------------------------------------

applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

main6 :: IO ()
main6 = do
    print $ applyToMaybe (*2) (Just 5)   -- Just 10
    print $ applyToMaybe (*2) Nothing   -- Nothing

------------------------------------------------------------
-- HC18T7: fmapTuple using Functor instance of (,) a
------------------------------------------------------------

fmapTuple :: (a -> b) -> (x, a) -> (x, b)
fmapTuple = fmap

main7 :: IO ()
main7 = do
    print $ fmapTuple (+1) ("Count", 4)  -- ("Count", 5)

------------------------------------------------------------
-- HC18T8: identityLawCheck
------------------------------------------------------------

identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x

main8 :: IO ()
main8 = do
    print $ identityLawCheck (Just 10)          -- True
    print $ identityLawCheck [1,2,3]            -- True
    print $ identityLawCheck (Right "Hi" :: Either String String) -- True

------------------------------------------------------------
-- HC18T9: compositionLawCheck
------------------------------------------------------------

compositionLawCheck :: (Eq (f c), Functor f) =>
    (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

main9 :: IO ()
main9 = do
    print $ compositionLawCheck (*2) (+3) (Just 4)     -- True
    print $ compositionLawCheck (++ "!") reverse ["abc"] -- True

------------------------------------------------------------
-- HC18T10: nestedFmap
------------------------------------------------------------

nestedFmap :: (a -> b) -> [[Maybe a]] -> [[Maybe b]]
nestedFmap = fmap . fmap . fmap

main10 :: IO ()
main10 = do
    let input = [[Just 1, Nothing], [Just 2]]
    print $ nestedFmap (+1) input  -- [[Just 2, Nothing], [Just 3]]

------------------------------------------------------------
-- Choose which main to run
------------------------------------------------------------

main :: IO ()
main = main1  -- change to main2, main3, ..., main10
