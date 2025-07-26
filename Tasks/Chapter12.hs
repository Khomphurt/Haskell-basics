-- HC12T1: Print a Welcome Message
welcomeMessage :: IO ()
welcomeMessage = putStrLn "Welcome to Haskell Programming!"

main1 :: IO ()
main1 = welcomeMessage

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

main2 :: IO ()
main2 = do
    putStrLn "Enter first number:"
    x <- readLn
    putStrLn "Enter second number:"
    y <- readLn
    print (addTwoNumbers x y)

-- HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main3 :: IO ()
main3 = do
    putStrLn "Enter a positive integer:"
    n <- readLn
    print (factorial n)

-- HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main4 :: IO ()
main4 = print [fibonacci n | n <- [0..9]]

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = cleaned == reverse cleaned
    where cleaned = map toLower $ filter (/= ' ') str

main5 :: IO ()
main5 = do
    putStrLn "Enter a string to check for palindrome:"
    input <- getLine
    putStrLn $ if isPalindrome input then "Palindrome" else "Not a palindrome"

-- Required for string manipulation
import Data.Char (toLower)

-- HC12T6: Sort a List of Integers
import Data.List (sort)

main6 :: IO ()
main6 = do
    putStrLn "Enter integers separated by spaces:"
    line <- getLine
    let numbers = map read (words line) :: [Int]
    print (sort numbers)

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

main7 :: IO ()
main7 = do
    putStrLn "Enter radius:"
    r <- readLn
    print (calculateCircleArea r)

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y    = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

main8 :: IO ()
main8 = do
    putStrLn "Enter first sorted list (space-separated):"
    list1 <- fmap (map read . words) getLine :: IO [Int]
    putStrLn "Enter second sorted list (space-separated):"
    list2 <- fmap (map read . words) getLine :: IO [Int]
    print (mergeLists list1 list2)

-- HC12T9: Read and Print File Content
import System.IO
import System.IO.Error (catchIOError)

readFileSafely :: FilePath -> IO ()
readFileSafely path = catchIOError
    (readFile path >>= putStrLn)
    (\e -> putStrLn $ "Error: " ++ show e)

main9 :: IO ()
main9 = do
    putStrLn "Enter file path:"
    path <- getLine
    readFileSafely path

-- HC12T10: Mathematical Operations Module

-- Define a module (simulated here as a section in the same file)
module MathOps where

add :: Int -> Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

subtract' :: Int -> Int -> Int
subtract' x y = x - y

-- Use it in main
main10 :: IO ()
main10 = do
    putStrLn "MathOps demo:"
    print ("Add 3 + 4 = " ++ show (MathOps.add 3 4))
    print ("Multiply 5 * 6 = " ++ show (MathOps.multiply 5 6))
    print ("Subtract 10 - 2 = " ++ show (MathOps.subtract' 10 2))
