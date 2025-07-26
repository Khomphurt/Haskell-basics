------------------------------------------------------------
-- HC19T1: Applicative Instance for Pair
------------------------------------------------------------

data Pair a = Pair a a deriving (Show, Functor)

instance Applicative Pair where
    pure x = Pair x x
    Pair f1 f2 <*> Pair x1 x2 = Pair (f1 x1) (f2 x2)

main1 :: IO ()
main1 = print $ Pair (+1) (*2) <*> Pair 10 20  -- Pair 11 40

------------------------------------------------------------
-- HC19T2: addThreeApplicative
------------------------------------------------------------

addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)

main2 :: IO ()
main2 = print $ addThreeApplicative (Just 1) (Just 2) (Just 3)

------------------------------------------------------------
-- HC19T3: safeProduct
------------------------------------------------------------

safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequenceA

main3 :: IO ()
main3 = do
    print $ safeProduct [Just 2, Just 3, Just 4]
    print $ safeProduct [Just 2, Nothing, Just 4]

------------------------------------------------------------
-- HC19T4: liftAndMultiply
------------------------------------------------------------

liftAndMultiply :: Int -> Int -> Int
liftAndMultiply = liftA2 (*)

main4 :: IO ()
main4 = print $ liftAndMultiply 3 4

------------------------------------------------------------
-- HC19T5: applyEffects
------------------------------------------------------------

applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (io1, io2) = (+) <$> io1 <*> io2

main5 :: IO ()
main5 = do
    let io1 = do putStrLn "Enter first number:"; readLn
    let io2 = do putStrLn "Enter second number:"; readLn
    result <- applyEffects (io1, io2)
    putStrLn $ "Sum is " ++ show result

------------------------------------------------------------
-- HC19T6: repeatEffect with forever
------------------------------------------------------------

repeatEffect :: IO () -> IO ()
repeatEffect = forever

main6 :: IO ()
main6 = repeatEffect $ putStrLn "Repeating... (Press Ctrl+C to stop)"

------------------------------------------------------------
-- HC19T7: conditionalPrint with when
------------------------------------------------------------

conditionalPrint :: Bool -> IO ()
conditionalPrint cond = when cond $ putStrLn "Condition is True!"

main7 :: IO ()
main7 = conditionalPrint True

------------------------------------------------------------
-- HC19T8: discardSecond with <*
------------------------------------------------------------

discardSecond :: IO a -> IO b -> IO a
discardSecond = (<*)

main8 :: IO ()
main8 = discardSecond (putStrLn "First") (putStrLn "Second")

------------------------------------------------------------
-- HC19T9: pureAndApply
------------------------------------------------------------

pureAndApply :: Maybe Int
pureAndApply = pure (+3) <*> Just 5

main9 :: IO ()
main9 = print pureAndApply

------------------------------------------------------------
-- HC19T10: combineResults for Either
------------------------------------------------------------

combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)

main10 :: IO ()
main10 = do
    print $ combineResults (Right 4) (Right 6)
    print $ combineResults (Left "Error") (Right 3)

------------------------------------------------------------
-- Main selector
------------------------------------------------------------

main :: IO ()
main = main1  -- Change to main2, main3, ..., main10 to test other tasks
