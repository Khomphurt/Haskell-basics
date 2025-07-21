module Main where

-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to value = "Transaction from " ++ from ++ " to " ++ to ++ " with value: " ++ show value

mainGenerateTx :: IO ()
mainGenerateTx = do
    print (generateTx "Alice" "Bob" 100)  -- "Transaction from Alice to Bob with value: 100"

-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person { name :: String, address :: (String, Int), paymentMethod :: PaymentMethod } deriving Show

bob :: Person
bob = Person { name = "Bob", address = ("123 Main St", 12345), paymentMethod = Cash }

mainPerson :: IO ()
mainPerson = do
    print bob  -- Person {name = "Bob", address = ("123 Main St", 12345), paymentMethod = Cash}

-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle l w) = l * w

mainArea :: IO ()
mainArea = do
    print (area (Circle 5))           -- 78.53981633974483
    print (area (Rectangle 10 5))     -- 50.0

-- HC8T4: Record Syntax for Employee
data Employee = Employee { empName :: String, experienceInYears :: Float } deriving Show

richard :: Employee
richard = Employee { empName = "Richard", experienceInYears = 7.5 }

mainEmployee :: IO ()
mainEmployee = do
    print richard  -- Employee {empName = "Richard", experienceInYears = 7.5}

-- HC8T5: Record Syntax for Person
data PersonRecord = PersonRecord { personName :: String, age :: Int, isEmployed :: Bool } deriving Show

person1 :: PersonRecord
person1 = PersonRecord { personName = "John", age = 30, isEmployed = True }

person2 :: PersonRecord
person2 = PersonRecord { personName = "Jane", age = 25, isEmployed = False }

mainPersonRecord :: IO ()
mainPersonRecord = do
    print person1  -- PersonRecord {personName = "John", age = 30, isEmployed = True}
    print person2  -- PersonRecord {personName = "Jane", age = 25, isEmployed = False}

-- HC8T6: Record Syntax for Shape Variants
data ShapeRecord = CircleRecord { center :: (Float, Float), color :: String, radius :: Float }
                 | RectangleRecord { center :: (Float, Float), color :: String, width :: Float, height :: Float } 
                 deriving Show

circleShape :: ShapeRecord
circleShape = CircleRecord { center = (0, 0), color = "Red", radius = 5.0 }

rectangleShape :: ShapeRecord
rectangleShape = RectangleRecord { center = (0, 0), color = "Blue", width = 10, height = 5 }

mainShapeRecord :: IO ()
mainShapeRecord = do
    print circleShape    -- CircleRecord {center = (0.0,0.0), color = "Red", radius = 5.0}
    print rectangleShape -- RectangleRecord {center = (0.0,0.0), color = "Blue", width = 10.0, height = 5.0}

-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "A dog named " ++ name
describeAnimal (Cat name) = "A cat named " ++ name

dogInstance :: Animal
dogInstance = Dog "Rex"

catInstance :: Animal
catInstance = Cat "Whiskers"

mainDescribeAnimal :: IO ()
mainDescribeAnimal = do
    print (describeAnimal dogInstance)  -- "A dog named Rex"
    print (describeAnimal catInstance)  -- "A cat named Whiskers"

-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, " ++ n ++ "! You are " ++ show a ++ " years old."

mainGreet :: IO ()
mainGreet = do
    print (greet "Alice" 30)  -- "Hello, Alice! You are 30 years old."

-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction { from :: Address, to :: Address, amount :: Value, transactionId :: String } deriving Show

createTransaction :: Address -> Address -> Value -> String -> Transaction
createTransaction from to value transactionId = Transaction { from = from, to = to, amount = value, transactionId = transactionId }

mainTransaction :: IO ()
mainTransaction = do
    let transaction = createTransaction "Alice" "Bob" 100 "TX12345"
    print transaction  -- Transaction {from = "Alice", to = "Bob", amount = 100, transactionId = "TX12345"}

-- HC8T10: Deriving Show for Book
data Book = Book { title :: String, author :: String, year :: Int } deriving Show

bookInstance :: Book
bookInstance = Book { title = "Haskell Programming", author = "John Doe", year = 2023 }

mainBook :: IO ()
mainBook = do
    print bookInstance  -- Book {title = "Haskell Programming", author = "John Doe", year = 2023}

-- Running all the test cases
main :: IO ()
main = do
    putStrLn "Testing generateTx Function:"
    mainGenerateTx
    putStrLn "\nTesting Person and PaymentMethod:"
    mainPerson
    putStrLn "\nTesting Area Function for Shapes:"
    mainArea
    putStrLn "\nTesting Employee Record:"
    mainEmployee
    putStrLn "\nTesting PersonRecord:"
    mainPersonRecord
    putStrLn "\nTesting ShapeRecord:"
    mainShapeRecord
    putStrLn "\nTesting DescribeAnimal Function:"
    mainDescribeAnimal
    putStrLn "\nTesting Greet Function:"
    mainGreet
    putStrLn "\nTesting Transaction Function:"
    mainTransaction
    putStrLn "\nTesting Book Show Instance:"
    mainBook
