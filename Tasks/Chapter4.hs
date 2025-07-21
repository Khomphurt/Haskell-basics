{-# LANGUAGE OverloadedStrings #-}

-- HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny" = "It's a bright and beautiful day!"
weatherReport "rainy" = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _ = "Weather unknown"

mainWeatherReport :: IO ()
mainWeatherReport = do
    putStrLn $ weatherReport "sunny"
    putStrLn $ weatherReport "rainy"
    putStrLn $ weatherReport "cloudy"
    putStrLn $ weatherReport "foggy"

-- HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday" = "It's a weekend!"
dayType _ = "It's a weekday."

mainDayType :: IO ()
mainDayType = do
    putStrLn $ dayType "Saturday"
    putStrLn $ dayType "Monday"
    putStrLn $ dayType "Sunday"
    putStrLn $ dayType "Wednesday"

-- HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent!"
    | grade >= 70 && grade <= 89  = "Good job!"
    | grade >= 50 && grade <= 69  = "You passed."
    | grade >= 0 && grade <= 49   = "Better luck next time."
    | otherwise = "Invalid grade"

mainGradeComment :: IO ()
mainGradeComment = do
    putStrLn $ gradeComment 95
    putStrLn $ gradeComment 80
    putStrLn $ gradeComment 65
    putStrLn $ gradeComment 45
    putStrLn $ gradeComment (-1)

-- HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching
specialBirthday :: Int -> String
specialBirthday 16 = "Sweet 16!"
specialBirthday 18 = "You can vote now!"
specialBirthday 21 = "Time to celebrate adulthood!"
specialBirthday age = "Happy " ++ show age ++ "th birthday!"

mainSpecialBirthday :: IO ()
mainSpecialBirthday = do
    putStrLn $ specialBirthday 16
    putStrLn $ specialBirthday 18
    putStrLn $ specialBirthday 21
    putStrLn $ specialBirthday 30

-- HC4T5 - Task 5: Add a Catch-All Pattern with a Custom Message
specialBirthdayWithAge :: Int -> String
specialBirthdayWithAge 16 = "Sweet 16!"
specialBirthdayWithAge 18 = "You can vote now!"
specialBirthdayWithAge 21 = "Time to celebrate adulthood!"
specialBirthdayWithAge age = "Happy " ++ show age ++ "th birthday! Hope you have a great year!"

mainSpecialBirthdayWithAge :: IO ()
mainSpecialBirthdayWithAge = do
    putStrLn $ specialBirthdayWithAge 16
    putStrLn $ specialBirthdayWithAge 18
    putStrLn $ specialBirthdayWithAge 21
    putStrLn $ specialBirthdayWithAge 50

-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList [] = "The list is empty."
whatsInsideThisList [x] = "The list has one element."
whatsInsideThisList [x, y] = "The list has two elements."
whatsInsideThisList _ = "The list has more than two elements."

mainWhatsInsideThisList :: IO ()
mainWhatsInsideThisList = do
    putStrLn $ whatsInsideThisList []
    putStrLn $ whatsInsideThisList [1]
    putStrLn $ whatsInsideThisList [1, 2]
    putStrLn $ whatsInsideThisList [1, 2, 3, 4]

-- HC4T7 - Task 7: Ignore Elements in a List
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _ = []

mainFirstAndThird :: IO ()
mainFirstAndThird = do
    print $ firstAndThird [1, 2, 3, 4]
    print $ firstAndThird [5, 6, 7]
    print $ firstAndThird [8, 9]
    print $ firstAndThird [10]

-- HC4T8 - Task 8: Extract Values from Tuples
describeTuple :: (Show a, Show b) => (a, b) -> String
describeTuple (x, y) = "The first value is " ++ show x ++ " and the second value is " ++ show y

mainDescribeTuple :: IO ()
mainDescribeTuple = do
    putStrLn $ describeTuple (5, "hello")
    putStrLn $ describeTuple (True, 3.14)
    putStrLn $ describeTuple ('a', [1, 2, 3])

-- Run all tests
main :: IO ()
main = do
    putStrLn "Testing weatherReport:"
    mainWeatherReport
    putStrLn "\nTesting dayType:"
    mainDayType
    putStrLn "\nTesting gradeComment:"
    mainGradeComment
    putStrLn "\nTesting specialBirthday:"
    mainSpecialBirthday
    putStrLn "\nTesting specialBirthdayWithAge:"
    mainSpecialBirthdayWithAge
    putStrLn "\nTesting whatsInsideThisList:"
    mainWhatsInsideThisList
    putStrLn "\nTesting firstAndThird:"
    mainFirstAndThird
    putStrLn "\nTesting describeTuple:"
    mainDescribeTuple
