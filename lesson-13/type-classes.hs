import GHC.Base (VecElem (Int16ElemRep))

class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- Homework
-- 1.
minInt = minBound :: Int

maxInt = maxBound :: Int

minWord = minBound :: Word

maxWord = maxBound :: Word

-- 2.
inc :: Int -> Int
inc x = x + 1

r1 = succ maxBound :: Int -- will throw

r2 = inc maxBound :: Int -- will rotate - will be equal to minBound :: Int

-- 3.
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound
    then minBound
    else succ n