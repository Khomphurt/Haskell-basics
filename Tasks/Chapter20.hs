------------------------------------------------------------
-- HC20T1: safeDivide with Maybe Monad
------------------------------------------------------------

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

main1 :: IO ()
main1 = do
  print $ safeDivide 10 2
  print $ safeDivide 5 0

------------------------------------------------------------
-- HC20T2: sequenceMaybe for List of Maybe
------------------------------------------------------------

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequenceA

main2 :: IO ()
main2 = do
  print $ sequenceMaybe [Just 1, Just 2, Just 3]
  print $ sequenceMaybe [Just 1, Nothing, Just 3]

------------------------------------------------------------
-- HC20T3: Writer Monad Logging Calculator
------------------------------------------------------------

type Log = [String]
type Logger = Writer Log

addLogged :: Int -> Int -> Logger Int
addLogged x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

subLogged :: Int -> Int -> Logger Int
subLogged x y = writer (x - y, ["Subtracted " ++ show y ++ " from " ++ show x])

mulLogged :: Int -> Int -> Logger Int
mulLogged x y = writer (x * y, ["Multiplied " ++ show x ++ " and " ++ show y])

calculator :: Logger Int
calculator = do
  a <- addLogged 5 3
  b <- subLogged a 2
  mulLogged b 4

main3 :: IO ()
main3 = do
  let (result, logs) = runWriter calculator
  putStrLn $ "Result: " ++ show result
  putStrLn "Logs:"
  mapM_ putStrLn logs

------------------------------------------------------------
-- HC20T4: countChars with State Monad
------------------------------------------------------------

countChars :: Char -> String -> Int
countChars c s = execState (mapM_ countChar s) 0
  where
    countChar x = when (x == c) $ modify (+1)

main4 :: IO ()
main4 = print $ countChars 'l' "hello world"

------------------------------------------------------------
-- HC20T5: Reader Monad for Configurable Greeting
------------------------------------------------------------

newtype Config = Config { greeting :: String }

greetUser :: Reader Config String
greetUser = do
  cfg <- ask
  return $ greeting cfg ++ ", user!"

main5 :: IO ()
main5 = do
  let cfg = Config "Hello"
  putStrLn $ runReader greetUser cfg

------------------------------------------------------------
-- HC20T6: doubleMonad Combining Maybe and List
------------------------------------------------------------

doubleMonad :: Maybe a -> [a]
doubleMonad Nothing = []
doubleMonad (Just x) = [x, x]

main6 :: IO ()
main6 = do
  print $ doubleMonad (Just 5)
  print $ doubleMonad Nothing

------------------------------------------------------------
-- HC20T7: findFirst with Either Monad
------------------------------------------------------------

findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "No matching element found"
findFirst p (x:xs) = if p x then Right x else findFirst p xs

main7 :: IO ()
main7 = do
  print $ findFirst (>5) [1,3,7,9]
  print $ findFirst (<0) [1,2,3]

------------------------------------------------------------
-- HC20T8: Parser Monad for Simple Expressions
------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (x, rest) <- runParser p input
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  pf <*> px = Parser $ \input -> do
    (f, rest1) <- runParser pf input
    (x, rest2) <- runParser px rest1
    return (f x, rest2)

instance Monad Parser where
  p >>= f = Parser $ \input -> do
    (x, rest1) <- runParser p input
    runParser (f x) rest1

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs) | x == c = Just (c, xs)
    f _ = Nothing

digitP :: Parser Int
digitP = Parser f
  where
    f (x:xs) | x >= '0' && x <= '9' = Just (read [x], xs)
    f _ = Nothing

exprP :: Parser Int
exprP = do
  a <- digitP
  _ <- charP '+'
  b <- digitP
  return (a + b)

main8 :: IO ()
main8 = do
  print $ runParser exprP "3+4"
  print $ runParser exprP "9+2"
  print $ runParser exprP "5-1"

------------------------------------------------------------
-- HC20T9: replicateMonad with Identity Monad
------------------------------------------------------------

replicateMonad :: Monad m => Int -> a -> m [a]
replicateMonad n x = replicateM n (return x)

main9 :: IO ()
main9 = print $ runIdentity $ replicateMonad 3 "Hi"

------------------------------------------------------------
-- HC20T10: Nested StateT and MaybeT Transformer
------------------------------------------------------------

type StateMaybe s a = StateT s Maybe a

safeDiv :: Int -> Int -> StateMaybe Int Int
safeDiv _ 0 = lift Nothing
safeDiv x y = do
  modify (+1)
  return (x `div` y)

main10 :: IO ()
main10 = do
  print $ runStateT (safeDiv 10 2) 0 -- Just (5,1)
  print $ runStateT (safeDiv 10 0) 0 -- Nothing

------------------------------------------------------------
-- HC20T11: randomWalk with State Monad
------------------------------------------------------------

type Position = (Int, Int)

randomStep :: State StdGen Position
randomStep = do
  (x,y) <- getPos
  dx <- state $ randomR (-1,1)
  dy <- state $ randomR (-1,1)
  let newPos = (x+dx, y+dy)
  putPos newPos
  return newPos
  where
    getPos = get
    putPos = put

randomWalk :: Int -> Position -> StdGen -> [Position]
randomWalk n start gen = evalState (replicateM n randomStep) gen
  where
    get = state $ \s -> (s, s)
    put = state $ \_ -> ((), start)

main11 :: IO ()
main11 = do
  gen <- getStdGen
  print $ randomWalk 5 (0,0) gen

------------------------------------------------------------
-- HC20T12: File Reading with IO Monad
------------------------------------------------------------

main12 :: IO ()
main12 = do
  putStrLn "Enter filename to read:"
  fname <- getLine
  contents <- readFile fname
  putStr contents

------------------------------------------------------------
-- HC20T13: fibonacciMemo with State Monad
------------------------------------------------------------

fibonacciMemo :: Int -> State [(Int, Integer)] Integer
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
  memo <- get
  case lookup n memo of
    Just val -> return val
    Nothing -> do
      a <- fibonacciMemo (n-1)
      b <- fibonacciMemo (n-2)
      let val = a + b
      modify ((n,val):)
      return val

main13 :: IO ()
main13 = print $ evalState (fibonacciMemo 20) []

------------------------------------------------------------
-- HC20T14: mapMFilter Monadic Map-Filter
------------------------------------------------------------

mapMFilter :: Monad m => (a -> m Bool) -> [a] -> m [a]
mapMFilter p [] = return []
mapMFilter p (x:xs) = do
  keep <- p x
  ys <- mapMFilter p xs
  return $ if keep then x:ys else ys

main14 :: IO ()
main14 = print =<< mapMFilter (\x -> return (x > 2)) [1..5]

------------------------------------------------------------
-- HC20T15: treeSum with Custom Monad
------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

newtype SumM a = SumM { runSumM :: a } deriving (Num)

instance Monad SumM where
  return = SumM
  SumM x >>= f = f x

treeSum :: Num a => Tree a -> a
treeSum EmptyT = 0
treeSum (NodeT x l r) = x + treeSum l + treeSum r

main15 :: IO ()
main15 = do
  let tree = NodeT 5 (NodeT 3 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT)
  print $ treeSum tree

------------------------------------------------------------
-- HC20T16: retryIO with IO Monad
------------------------------------------------------------

retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = do
  r <- action
  return (Just r) `catch` \_ -> retryIO (n-1) action
  where
    catch = Control.Exception.catch

main16 :: IO ()
main16 = do
  let failAction = fail "fail"
  r <- retryIO 3 failAction
  print r

------------------------------------------------------------
-- HC20T17: validatePassword with Either Monad
------------------------------------------------------------

validatePassword :: String -> Either [String] String
validatePassword pw = case errors of
  [] -> Right pw
  _  -> Left errors
  where
    errors = concat
      [ if length pw < 8 then ["Password too short"] else []
      , if not (any (`elem` ['0'..'9']) pw) then ["Must contain digit"] else []
      , if not (any (`elem` ['A'..'Z']) pw) then ["Must contain uppercase"] else []
      ]

main17 :: IO ()
main17 = do
  print $ validatePassword "Short1"
  print $ validatePassword "LongEnough1A"

------------------------------------------------------------
-- HC20T18: Maybe
