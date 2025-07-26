-- HC16T1: Reverse a String
reverseStr :: String -> String
reverseStr = reverse

main1 :: IO ()
main1 = do
    putStrLn "Enter a string to reverse:"
    s <- getLine
    putStrLn $ "Reversed: " ++ reverseStr s

-- HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

main2 :: IO ()
main2 = do
    putStrLn "Enter a string to check palindrome:"
    s <- getLine
    print $ isPalindrome s

-- HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main3 :: IO ()
main3 = do
    putStrLn "Enter a number to get factorial:"
    n <- readLn
    print $ factorial n

-- HC16T4: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

main4 :: IO ()
main4 = do
    putStrLn "Enter integers separated by spaces:"
    input <- getLine
    let nums = map read $ words input
    print $ filterEven nums

-- HC16T5: Uppercase String
toUpperStr :: String -> String
toUpperStr = map toUpper

main5 :: IO ()
main5 = do
    putStrLn "Enter a string to convert to uppercase:"
    s <- getLine
    putStrLn $ toUpperStr s

-- HC16T6: nth Fibonacci Number
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main6 :: IO ()
main6 = do
    putStrLn "Enter n to get nth Fibonacci number:"
    n <- readLn
    print $ fib n

-- HC16T7: Element Existence in List
exists :: Eq a => a -> [a] -> Bool
exists = elem

main7 :: IO ()
main7 = do
    putStrLn "Enter list elements (space-separated):"
    lst <- fmap words getLine
    putStrLn "Enter element to check:"
    e <- getLine
    print $ exists e lst

-- HC16T8: Insertion Sort
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
        | y <= z    = y : z : zs
        | otherwise = z : insert y zs

main8 :: IO ()
main8 = do
    putStrLn "Enter integers to sort (space-separated):"
    input <- getLine
    let nums = map read $ words input
    print $ insertSort nums

-- HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

main9 :: IO ()
main9 = do
    putStrLn "Enter a list with duplicates:"
    input <- getLine
    print $ removeDuplicates (words input)

-- HC16T10: Character Frequency in String
charFreq :: String -> [(Char, Int)]
charFreq str = map (\grp -> (head grp, length grp)) . group . sort $ str

main10 :: IO ()
main10 = do
    putStrLn "Enter a string:"
    s <- getLine
    print $ charFreq s

-- Change this to main1..main10 to test each function
main :: IO ()
main = main1
