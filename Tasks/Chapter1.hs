-- HC1.hs

import Data.List (sortBy)

-------------------------------------
-- HC1T1 - Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main1 :: IO ()
main1 = do
    let x = 5
    putStrLn $ "doubleThenIncrement " ++ show x ++ " = " ++ show (doubleThenIncrement x)

-------------------------------------
-- HC1T2 - Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

main2 :: IO ()
main2 = do
    let r = 3.0
    putStrLn $ "circleArea " ++ show r ++ " = " ++ show (circleArea r)

-------------------------------------
-- HC1T3 - Check if Greater Than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main3 :: IO ()
main3 = do
    let x = 20
    putStrLn $ "greaterThan18 " ++ show x ++ " = " ++ show (greaterThan18 x)

-------------------------------------
-- HC1T4 - Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers = map fst

sortByScore :: [Player] -> [Player]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main4 :: IO ()
main4 = do
    let players = [("Alice", 90), ("Bob", 70), ("Charlie", 95), ("Dana", 80)]
    putStrLn "Top 3 Players:"
    mapM_ putStrLn (getTopThreePlayers players)

-------------------------------------
-- HC1T5 - Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

takeN :: Int -> [Int]
takeN n = take n infiniteNumbers

main5 :: IO ()
main5 = do
    let n = 10
    putStrLn $ "First " ++ show n ++ " numbers: " ++ show (takeN n)

-------------------------------------
-- HC1T6 - Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main6 :: IO ()
main6 = do
    let x = 10
        y = 20
    putStrLn $ show x ++ " + " ++ show y ++ " = " ++ show (addNumbers x y)

-------------------------------------
-- HC1T7 - Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9

main7 :: IO ()
main7 = do
    let f = 98.6
    putStrLn $ show f ++ "°F = " ++ show (fToC f) ++ "°C"

-------------------------------------
-- HC1T8 - Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main8 :: IO ()
main8 = do
    let x = 3
    putStrLn $ "applyTwice (+1) " ++ show x ++ " = " ++ show (applyTwice (+1) x)

-------------------------------------
-- Change this to main1, main2, ..., main8 to test a specific task
main :: IO ()
main = main1
