-- HC3.hs

-------------------------------------
-- HC3T1 - Task 1: Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber x
    | x > 0     = "Positive"
    | x < 0     = "Negative"
    | otherwise = "Zero"

main1 :: IO ()
main1 = do
    putStrLn $ "checkNumber 5 = " ++ checkNumber 5
    putStrLn $ "checkNumber (-3) = " ++ checkNumber (-3)
    putStrLn $ "checkNumber 0 = " ++ checkNumber 0

-------------------------------------
-- HC3T2 - Task 2: Determine the grade based on a score using guards
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"

main2 :: IO ()
main2 = do
    putStrLn $ "grade 95 = " ++ grade 95
    putStrLn $ "grade 72 = " ++ grade 72
    putStrLn $ "grade 50 = " ++ grade 50

-------------------------------------
-- HC3T3 - Task 3: Convert an RGB color to a hex string using let bindings
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) = "#" ++ hex r ++ hex g ++ hex b
  where
    hex x = let hexStr = showHex x "" in if length hexStr == 1 then "0" ++ hexStr else hexStr

main3 :: IO ()
main3 = do
    putStrLn $ "rgbToHex (255, 0, 127) = " ++ rgbToHex (255, 0, 127)
    putStrLn $ "rgbToHex (0, 255, 64) = " ++ rgbToHex (0, 255, 64)

-------------------------------------
-- HC3T4 - Task 4: Calculate the area of a triangle using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt(s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

main4 :: IO ()
main4 = do
    putStrLn $ "triangleArea 3 4 5 = " ++ show (triangleArea 3 4 5)
    putStrLn $ "triangleArea 7 8 9 = " ++ show (triangleArea 7 8 9)

-------------------------------------
-- HC3T5 - Task 5: Determine the type of a triangle using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

main5 :: IO ()
main5 = do
    putStrLn $ "triangleType 3 3 3 = " ++ triangleType 3 3 3
    putStrLn $ "triangleType 5 5 8 = " ++ triangleType 5 5 8
    putStrLn $ "triangleType 6 7 8 = " ++ triangleType 6 7 8

-------------------------------------
-- HC3T6 - Task 6: Check leap year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0   = True
    | otherwise           = False

main6 :: IO ()
main6 = do
    putStrLn $ "isLeapYear 2000 = " ++ show (isLeapYear 2000)
    putStrLn $ "isLeapYear 1900 = " ++ show (isLeapYear 1900)
    putStrLn $ "isLeapYear 2024 = " ++ show (isLeapYear 2024)

-------------------------------------
-- HC3T7 - Task 7: Determine the season based on the month using guards
season :: Int -> String
season month
    | month == 12 || month == 1 || month == 2 = "Winter"
    | month >= 3 && month <= 5 = "Spring"
    | month >= 6 && month <= 8 = "Summer"
    | month >= 9 && month <= 11 = "Autumn"
    | otherwise = "Invalid month"

main7 :: IO ()
main7 = do
    putStrLn $ "season 3 = " ++ season 3
    putStrLn $ "season 7 = " ++ season 7
    putStrLn $ "season 11 = " ++ season 11

-------------------------------------
-- HC3T8 - Task 8: Calculate BMI and return category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25 = "Normal"
    | bmi < 30 = "Overweight"
    | otherwise = "Obese"
  where
    bmi = weight / (height * height)

main8 :: IO ()
main8 = do
    putStrLn $ "bmiCategory 70 1.75 = " ++ bmiCategory 70 1.75
    putStrLn $ "bmiCategory 90 1.8 = " ++ bmiCategory 90 1.8

-------------------------------------
-- HC3T9 - Task 9: Find the maximum of three numbers using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = let max1 = max x y; max2 = max max1 z in max2

main9 :: IO ()
main9 = do
    putStrLn $ "maxOfThree 10 20 15 = " ++ show (maxOfThree 10 20 15)
    putStrLn $ "maxOfThree 5 25 10 = " ++ show (maxOfThree 5 25 10)

-------------------------------------
-- HC3T10 - Task 10: Check if a string is a palindrome using recursion and guards
isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1 = True
    | head str == last str = isPalindrome (init (tail str))
    | otherwise = False

main10 :: IO ()
main10 = do
    putStrLn $ "isPalindrome \"racecar\" = " ++ show (isPalindrome "racecar")
    putStrLn $ "isPalindrome \"haskell\" = " ++ show (isPalindrome "haskell")
    putStrLn $ "isPalindrome \"madam\" = " ++ show (isPalindrome "madam")

-------------------------------------
-- Change this to main1, main2, ..., main10 to test a specific task
main :: IO ()
main = main1

