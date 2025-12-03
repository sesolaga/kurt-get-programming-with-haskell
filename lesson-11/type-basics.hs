-- Doesn't work
-- myAverage xs = sum xs / length xs

x :: Int
x = 2

works = x * 2000

zero = x ^ 2000

y :: Integer
y = 2

res = y * 2000

worksForInteger = y ^ 2000

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1, 2, 3]

letters :: [Char]
letters = ['a', 'b', 'c']

isTrue = letters == "abc"

-- [Char] == String
aPet :: String
aPet = "dog"

-- Tuples - like a list but:
-- 1. Specific length
-- 2. Might have different types inside

ageAndHeight :: (Int, Int)
ageAndHeight = (36, 180)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Sergei", "Solagaian", 'V')

-- Function types

double :: Int -> Int
double x = x * 2

half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Int -> Int
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n * 2)

anotherNumber :: Int
anotherNumber = read "6"

z1 = read "3" :: Int

z2 = read "3" :: Double

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

makeAddressLambda =
  ( \number ->
      ( \street ->
          (\town -> (number, street, town))
      )
  )

addr1 = (((makeAddressLambda 123) "Happy St") "Haskell Town")
addr2 = (((makeAddress 123) "Happy St") "Haskell Town")

addr3 = makeAddressLambda 123 "Happy St" "Haskell Town"

-- f0 :: Int -> String -> String -> (Int, String, String)
-- f1 :: String -> String -> (Int, String, String)
-- f2 :: String -> (Int, String, String)


-- Homework

-- Filter type signature
-- filter :: (a -> Bool) -> [a] -> [a]

-- tail :: [a] -> [a] 
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- head :: [a] -> b 
safeHeadNotImplementableCorrectly [] = []
safeHeadNotImplementableCorrectly (x:xs) = x

-- myFoldl
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
