-- HC2.hs

-------------------------------------
-- HC2T1 - Checking Types in GHCi
-- Expressions to check types in GHCi
-- Type checks should be done manually in GHCi

main1 :: IO ()
main1 = do
    -- You can open GHCi and check the types of these expressions:
    putStrLn "Checking Types in GHCi:"
    -- Expected Types:
    -- 42 :: Num a => a
    -- 3.14 :: Fractional a => a
    -- "Haskell" :: [Char]
    -- 'Z' :: Char
    -- True && False :: Bool
    putStrLn "Check these in GHCi manually!"

-------------------------------------
-- HC2T2 - Function Type Signatures
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings str1 str2 = str1 ++ str2

main2 :: IO ()
main2 = do
    let a = 5
        b = 10
    putStrLn $ "add " ++ show a ++ " and " ++ show b ++ " = " ++ show (add a b)
    putStrLn $ "isEven " ++ show a ++ " = " ++ show (isEven a)
    putStrLn $ "concatStrings \"Hello \" \"World\" = " ++ concatStrings "Hello " "World"

-------------------------------------
-- HC2T3 - Immutable Variables
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

main3 :: IO ()
main3 = do
    putStrLn $ "myAge: " ++ show myAge
    putStrLn $ "piValue: " ++ show piValue
    putStrLn $ "greeting: " ++ greeting
    putStrLn $ "isHaskellFun: " ++ show isHaskellFun
    -- Try modifying one of these variables and observe that it will not work (immutable)

-------------------------------------
-- HC2T4 - Converting Between Infix and Prefix Notations

-- Infix to Prefix
prefixAddition :: Int -> Int -> Int
prefixAddition = (+)

prefixMultiplication :: Int -> Int -> Int
prefixMultiplication = (*)

prefixAnd :: Bool -> Bool -> Bool
prefixAnd = (&&)

main4 :: IO ()
main4 = do
    let infix1 = 5 + 3
    let infix2 = 10 * 4
    let infix3 = True && False

    putStrLn $ "Infix: 5 + 3 = " ++ show infix1
    putStrLn $ "Infix: 10 * 4 = " ++ show infix2
    putStrLn $ "Infix: True && False = " ++ show infix3

    putStrLn "Prefix Notation results:"
    putStrLn $ "(+) 7 2 = " ++ show (prefixAddition 7 2)
    putStrLn $ "(*) 6 5 = " ++ show (prefixMultiplication 6 5)
    putStrLn $ "(&&) True False = " ++ show (prefixAnd True False)

-------------------------------------
-- HC2T5 - Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

main5 :: IO ()
main5 = do
    let radius = 5.0
    putStrLn $ "circleArea with radius " ++ show radius ++ " = " ++ show (circleArea radius)

    let a = 10
        b = 20
        c = 15
    putStrLn $ "maxOfThree " ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ " = " ++ show (maxOfThree a b c)

-------------------------------------
-- HC2T6 - Understanding Int vs Integer
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

main6 :: IO ()
main6 = do
    let largeInt = 2 ^ 64 :: Int
    putStrLn $ "smallNumber: " ++ show smallNumber
    putStrLn $ "bigNumber: " ++ show bigNumber
    putStrLn $ "2^64 as Int: " ++ show largeInt
    -- The result of 2^64 :: Int may overflow. Try it in GHCi to observe this.

-------------------------------------
-- HC2T7 - Boolean Expressions

main7 :: IO ()
main7 = do
    let expr1 = True && True
    let expr2 = False || True
    let expr3 = not False
    let expr4 = 5 > 10

    putStrLn $ "True && True = " ++ show expr1
    putStrLn $ "False || True = " ++ show expr2
    putStrLn $ "not False = " ++ show expr3
    putStrLn $ "5 > 10 = " ++ show expr4

-------------------------------------
-- HC2T8 - Higher-Order Function (applyTwice)
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
