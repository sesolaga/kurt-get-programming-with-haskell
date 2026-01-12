import Data.List

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Ord, Enum)

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

instance Eq SixSidedDie where
  (==) S1 S1 = True
  (==) S2 S2 = True
  (==) S3 S3 = True
  (==) S4 S4 = True
  (==) S5 S5 = True
  (==) S6 S6 = True
  (==) _ _ = False

data Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names = [Name ("Emil", "Cioran"), Name ("Eugene", "Thacker"), Name ("Friedrich", "Nietzsche")]

sorted = sort names

instance Ord Name where
  compare (Name s1) (Name s2) = compare (snd s1) (snd s2)

-- Homework

-- 1.

data Number = One | Two | Three deriving Enum

instance Eq Number where
    (==) x y = fromEnum x == fromEnum y 

instance Ord Number where
    compare x y = compare (fromEnum x) (fromEnum y)

-- 2.

data FiveSidedDie = D1 | D2 | D3 | D4 | D5 deriving (Show, Eq, Enum)

class (Eq a, Enum a) => Die a where
    roll :: Int -> a  

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)

r1 = roll 3 :: FiveSidedDie
r2 = roll 5 :: FiveSidedDie