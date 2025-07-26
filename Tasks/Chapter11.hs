-- HC11T1: Greet the User
greetUser :: IO ()
greetUser = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

main1 :: IO ()
main1 = greetUser

-- HC11T2: Count Characters in a Line
countCharacters :: IO ()
countCharacters = do
    putStrLn "Enter a line:"
    line <- getLine
    putStrLn ("Number of characters: " ++ show (length line))

main2 :: IO ()
main2 = countCharacters

-- HC11T3: Double a Number
doubleNumber :: IO ()
doubleNumber = do
    putStrLn "Enter a number:"
    input <- getLine
    let num = read input :: Int
    putStrLn ("Double the number is: " ++ show (2 * num))

main3 :: IO ()
main3 = doubleNumber

-- HC11T4: Concatenate Two Lines
concatenateLines :: IO ()
concatenateLines = do
    putStrLn "Enter first line:"
    line1 <- getLine
    putStrLn "Enter second line:"
    line2 <- getLine
    putStrLn ("Concatenated: " ++ line1 ++ line2)

main4 :: IO ()
main4 = concatenateLines

-- HC11T5: Repeat Until "quit"
repeatUntilQuit :: IO ()
repeatUntilQuit = do
    putStrLn "Type something (or 'quit' to exit):"
    input <- getLine
    if input == "quit"
        then putStrLn "Goodbye!"
        else do
            putStrLn ("You typed: " ++ input)
            repeatUntilQuit

main5 :: IO ()
main5 = repeatUntilQuit

-- HC11T6: Uppercase Converter
uppercaseConverter :: IO ()
uppercaseConverter = do
    putStrLn "Enter a line:"
    line <- getLine
    putStrLn ("Uppercase: " ++ map toUpper line)

main6 :: IO ()
main6 = uppercaseConverter

-- Required for toUpper
import Data.Char (toUpper)

-- HC11T7: User Options
userOptions :: IO ()
userOptions = do
    putStrLn "Choose an option:"
    putStrLn "1. Say Hello"
    putStrLn "2. Say Goodbye"
    putStrLn "3. Exit"
    choice <- getLine
    case choice of
        "1" -> putStrLn "Hello!"
        "2" -> putStrLn "Goodbye!"
        "3" -> putStrLn "Exiting..."
        _   -> putStrLn "Invalid choice."

main7 :: IO ()
main7 = userOptions

-- HC11T8: Even or Odd Checker
evenOrOdd :: IO ()
evenOrOdd = do
    putStrLn "Enter a number:"
    input <- getLine
    let num = read input :: Int
    if even num
        then putStrLn "Even"
        else putStrLn "Odd"

main8 :: IO ()
main8 = evenOrOdd

-- HC11T9: Sum Two Numbers
sumTwoNumbers :: IO ()
sumTwoNumbers = do
    putStrLn "Enter first number:"
    input1 <- getLine
    putStrLn "Enter second number:"
    input2 <- getLine
    let num1 = read input1 :: Int
    let num2 = read input2 :: Int
    putStrLn ("Sum: " ++ show (num1 + num2))

main9 :: IO ()
main9 = sumTwoNumbers

-- HC11T10: Reverse User Input
reverseInput :: IO ()
reverseInput = do
    putStrLn "Enter a string:"
    str <- getLine
    putStrLn ("Reversed: " ++ reverse str)

main10 :: IO ()
main10 = reverseInput
