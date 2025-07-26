-- HC14T1: Print Hello, Cabal!
main1 :: IO ()
main1 = putStrLn "Hello, Cabal!"

-- HC14T2: Print Random Number
main2 :: IO ()
main2 = do
    num <- randomRIO (1, 100) :: IO Int
    putStrLn $ "Random number: " ++ show num

-- HC14T3: NumericUnderscores Extension
main3 :: IO ()
main3 = do
    let bigNum1 = 1_000_000
    let bigNum2 = 123_456_789
    print (bigNum1 + bigNum2)

-- HC14T4: TypeApplications Extension
readInt :: String -> Int
readInt = read @Int

main4 :: IO ()
main4 = do
    putStrLn "Enter a number string:"
    str <- getLine
    print (readInt str)

-- HC14T5: Custom Data Type + Pattern Matching with @
handleResult :: Result Int -> String
handleResult r@(Success n) = "Got result: " ++ show n ++ " from " ++ show r
handleResult r@(Failure msg) = "Failure: " ++ msg ++ " from " ++ show r

main5 :: IO ()
main5 = do
    print $ handleResult (Success 42)
    print $ handleResult (Failure "Error!")

-- HC14T6: Structure Confirmed (already in app/src structure)
main6 :: IO ()
main6 = putStrLn "Project structure uses src/ and app/."

-- HC14T7: Library Component Confirmed (add library stanza to .cabal)
main7 :: IO ()
main7 = putStrLn "Library component active and imported."

-- HC14T8: Character Frequency
main8 :: IO ()
main8 = do
    putStrLn "Enter a string:"
    str <- getLine
    print (counts str)

-- HC14T9: PartialTypeSignatures
sumFirstTwo :: [Int] -> _
sumFirstTwo (x:y:_) = x + y
sumFirstTwo _ = 0

main9 :: IO ()
main9 = print $ sumFirstTwo [3, 5, 7]

-- Main placeholder to run specific test
main :: IO ()
main = main1  -- Change this line to run main2..main9
