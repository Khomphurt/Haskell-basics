module Main where

import Prelude hiding (sum, map, length)

-- HC6T1: Factorial (Recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

mainFactorial :: IO ()
mainFactorial = do
    print $ factorial 5
    print $ factorial 0
    print $ factorial 10

-- HC6T2: Fibonacci (Recursive)
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

mainFibonacci :: IO ()
mainFibonacci = do
    print $ fibonacci 5
    print $ fibonacci 10
    print $ fibonacci 0

-- HC6T3: Sum of Elements Using foldr
sumList :: [Integer] -> Integer
sumList = foldr (+) 0

mainSumList :: IO ()
mainSumList = do
    print $ sumList [1, 2, 3, 4, 5]
    print $ sumList [10, 20, 30]
    print $ sumList []

-- HC6T4: Product of Elements Using foldl
productList :: [Integer] -> Integer
productList = foldl (*) 1

mainProductList :: IO ()
mainProductList = do
    print $ productList [1, 2, 3, 4, 5]
    print $ productList [10, 20, 30]
    print $ productList []

-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

mainReverseList :: IO ()
mainReverseList = do
    print $ reverseList [1, 2, 3, 4]
    print $ reverseList ["a", "b", "c"]
    print $ reverseList []

-- HC6T6: Element Exists in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists x (y:ys) = x == y || elementExists x ys

mainElementExists :: IO ()
mainElementExists = do
    print $ elementExists 3 [1, 2, 3, 4]
    print $ elementExists "apple" ["banana", "apple", "cherry"]
    print $ elementExists 10 [1, 2, 3, 4]

-- HC6T7: List Length
listLength :: [a] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

mainListLength :: IO ()
mainListLength = do
    print $ listLength [1, 2, 3, 4]
    print $ listLength ["a", "b", "c"]
    print $ listLength []

-- HC6T8: Filter Even Numbers
filterEven :: [Integer] -> [Integer]
filterEven = filter even

mainFilterEven :: IO ()
mainFilterEven = do
    print $ filterEven [1, 2, 3, 4, 5, 6]
    print $ filterEven [10, 15, 20, 25]
    print $ filterEven [1, 3, 5, 7]

-- HC6T9: Map Implementation
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

mainMapList :: IO ()
mainMapList = do
    print $ mapList (+1) [1, 2, 3, 4]
    print $ mapList (*2) [1, 2, 3, 4]
    print $ mapList show [1, 2, 3]

-- HC6T10: Digits of a Number (Recursive)
digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

mainDigits :: IO ()
mainDigits = do
    print $ digits 12345
    print $ digits 0
    print $ digits 987654321

-- Run all tests
main :: IO ()
main = do
    putStrLn "Testing factorial:"
    mainFactorial
    putStrLn "\nTesting fibonacci:"
    mainFibonacci
    putStrLn "\nTesting sumList:"
    mainSumList
    putStrLn "\nTesting productList:"
    mainProductList
    putStrLn "\nTesting reverseList:"
    mainReverseList
    putStrLn "\nTesting elementExists:"
    mainElementExists
    putStrLn "\nTesting listLength:"
    mainListLength
    putStrLn "\nTesting filterEven:"
    mainFilterEven
    putStrLn "\nTesting mapList:"
    mainMapList
    putStrLn "\nTesting digits:"
    mainDigits
