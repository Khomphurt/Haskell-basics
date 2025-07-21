{-# LANGUAGE OverloadedStrings #-}

module Main where

import PlutusTx
import PlutusTx.Prelude

-- HC5T1: Using applyTwice
applyTwice :: (Integer -> Integer) -> Integer -> Integer
applyTwice f x = f (f x)

mainApplyTwice :: IO ()
mainApplyTwice = do
    print $ applyTwice (+1) 5
    print $ applyTwice (*2) 3

-- HC5T2: Filtering Odd Numbers
filterOddNumbers :: [Integer] -> [Integer]
filterOddNumbers = filter odd

mainFilterOddNumbers :: IO ()
mainFilterOddNumbers = do
    print $ filterOddNumbers [1..30]

-- HC5T3: Checking for Uppercase Letters
checkUppercase :: [String] -> Bool
checkUppercase = any (\word -> head word == toUpper (head word))

mainCheckUppercase :: IO ()
mainCheckUppercase = do
    print $ checkUppercase ["hello", "world", "Haskell"]
    print $ checkUppercase ["hello", "world"]

-- HC5T4: Using Lambda Functions
biggerThan10 :: Integer -> Bool
biggerThan10 = (>) `flip` 10

mainBiggerThan10 :: IO ()
mainBiggerThan10 = do
    print $ biggerThan10 5
    print $ biggerThan10 15

-- HC5T5: Partial Application
multiplyByFive :: Integer -> Integer
multiplyByFive = (*5)

mainMultiplyByFive :: IO ()
mainMultiplyByFive = do
    print $ multiplyByFive 4
    print $ multiplyByFive 10

-- HC5T6: Function Composition
evenSquares :: [Integer] -> [Integer]
evenSquares = filter even . map (^2)

mainEvenSquares :: IO ()
mainEvenSquares = do
    print $ evenSquares [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- HC5T7: The $ Operator
result :: Integer
result = sum . map (*2) . filter (>3) $ [1..10]

mainResult :: IO ()
mainResult = do
    print result

-- HC5T8: Point-Free Style
addFive :: Integer -> Integer
addFive = (+5)

mainAddFive :: IO ()
mainAddFive = do
    print $ addFive 10
    print $ addFive 25

-- HC5T9: Higher-Order Function to Transform a List
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

mainTransformList :: IO ()
mainTransformList = do
    print $ transformList (+1) [1, 2, 3, 4]
    print $ transformList (*2) [5, 6, 7]

-- HC5T10: Combining Higher-Order Functions
checkSquaredGreaterThan50 :: [Integer] -> Bool
checkSquaredGreaterThan50 = any (>50) . map (^2)

mainCheckSquaredGreaterThan50 :: IO ()
mainCheckSquaredGreaterThan50 = do
    print $ checkSquaredGreaterThan50 [1, 2, 3, 4, 5]
    print $ checkSquaredGreaterThan50 [1, 2, 3]

-- Combining all in a single main for testing
main :: IO ()
main = do
    putStrLn "Testing applyTwice:"
    mainApplyTwice
    putStrLn "\nTesting filterOddNumbers:"
    mainFilterOddNumbers
    putStrLn "\nTesting checkUppercase:"
    mainCheckUppercase
    putStrLn "\nTesting biggerThan10:"
    mainBiggerThan10
    putStrLn "\nTesting multiplyByFive:"
    mainMultiplyByFive
    putStrLn "\nTesting evenSquares:"
    mainEvenSquares
    putStrLn "\nTesting result (using $ operator):"
    mainResult
    putStrLn "\nTesting addFive:"
    mainAddFive
    putStrLn "\nTesting transformList:"
    mainTransformList
    putStrLn "\nTesting checkSquaredGreaterThan50:"
    mainCheckSquaredGreaterThan50
