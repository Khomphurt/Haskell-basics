module Main where

import Prelude hiding (compare)

-- HC7T1: Implement an Eq Instance for a Custom Data Type
data Color = Red | Green | Blue deriving (Show)

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False

mainEq :: IO ()
mainEq = do
    print (Red == Red)    -- True
    print (Green == Blue) -- False
    print (Blue == Green) -- False

-- HC7T2: Implement an Ord Instance for a Custom Data Type
instance Ord Color where
    Red < Green = True
    Green < Blue = True
    Red < Blue = True
    _ < _ = False
    -- We define other comparisons implicitly, based on the order in the instance.

mainOrd :: IO ()
mainOrd = do
    print (Red < Green)  -- True
    print (Green < Blue) -- True
    print (Blue < Red)   -- False

-- HC7T3: Function Using Multiple Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
    | x == y    = x
    | x > y     = x
    | otherwise = y

mainCompareValues :: IO ()
mainCompareValues = do
    print (compareValues 3 5)    -- 5
    print (compareValues 'a' 'z') -- 'z'
    print (compareValues 7 7)     -- 7

-- HC7T4: Custom Type with Show and Read
data Shape = Circle Double | Rectangle Double Double deriving (Show, Read)

mainShape :: IO ()
mainShape = do
    print (Circle 5.0)              -- Circle 5.0
    print (Rectangle 4.0 6.0)       -- Rectangle 4.0 6.0
    print (read "Circle 5.0" :: Shape) -- Circle 5.0
    print (read "Rectangle 4.0 6.0" :: Shape) -- Rectangle 4.0 6.0

-- HC7T5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea side = side * side

mainSquareArea :: IO ()
mainSquareArea = do
    print (squareArea 4)    -- 16
    print (squareArea 5.5)  -- 30.25

-- HC7T6: Using Integral and Floating Type Classes
circleCircumference :: (Integral a, Floating b) => a -> b
circleCircumference radius = 2 * pi * fromIntegral radius

mainCircleCircumference :: IO ()
mainCircleCircumference = do
    print (circleCircumference 5 :: Double)  -- 31.41592653589793
    print (circleCircumference 5 :: Float)   -- 31.41593

-- HC7T7: Bounded and Enum
nextColor :: (Enum a, Bounded a) => a -> a
nextColor color
    | color == maxBound = minBound
    | otherwise         = succ color

mainNextColor :: IO ()
mainNextColor = do
    print (nextColor Red)   -- Green
    print (nextColor Green) -- Blue
    print (nextColor Blue)  -- Red

-- HC7T8: Parse a Value from a String Using Read
parseShape :: String -> Maybe Shape
parseShape s = case reads s of
    [(shape, "")] -> Just shape
    _             -> Nothing

mainParseShape :: IO ()
mainParseShape = do
    print (parseShape "Circle 5.0")                -- Just (Circle 5.0)
    print (parseShape "Rectangle 4.0 6.0")         -- Just (Rectangle 4.0 6.0)
    print (parseShape "Invalid shape")             -- Nothing

-- HC7T9: Type Class with Multiple Instances
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "It's True!"
    describe False = "It's False!"

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle l w) = "A rectangle with length " ++ show l ++ " and width " ++ show w

mainDescribable :: IO ()
mainDescribable = do
    print (describe True)             -- "It's True!"
    print (describe (Circle 5.0))     -- "A circle with radius 5.0"
    print (describe (Rectangle 4 6))  -- "A rectangle with length 4 and width 6"

-- HC7T10: Function with Multiple Type Class Constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y
    | x > y     = "The larger value is: " ++ describe x
    | x < y     = "The larger value is: " ++ describe y
    | otherwise = "Both are equal: " ++ describe x

mainDescribeAndCompare :: IO ()
mainDescribeAndCompare = do
    print (describeAndCompare True False)                   -- "The larger value is: It's True!"
    print (describeAndCompare (Circle 3.0) (Rectangle 4 4))  -- "The larger value is: A rectangle with length 4 and width 4"
    print (describeAndCompare (Rectangle 4 5) (Rectangle 4 5)) -- "Both are equal: A rectangle with length 4 and width 5"

-- Running all the test cases
main :: IO ()
main = do
    putStrLn "Testing Eq Instance for Color:"
    mainEq
    putStrLn "\nTesting Ord Instance for Color:"
    mainOrd
    putStrLn "\nTesting compareValues Function:"
    mainCompareValues
    putStrLn "\nTesting Shape Show and Read Instances:"
    mainShape
    putStrLn "\nTesting squareArea Function:"
    mainSquareArea
    putStrLn "\nTesting circleCircumference Function:"
    mainCircleCircumference
    putStrLn "\nTesting nextColor Function:"
    mainNextColor
    putStrLn "\nTesting parseShape Function:"
    mainParseShape
    putStrLn "\nTesting Describable Instances:"
    mainDescribable
    putStrLn "\nTesting describeAndCompare Function:"
    mainDescribeAndCompare
