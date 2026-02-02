import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int, Double)]
file1 = [(1, 200.1), (2, 199.5), (3, 199.4)
       , (4, 198.9), (5, 199.0), (6, 200.2)
       , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int, Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
       , (14, 203.5), (15, 204.9), (16, 207.1)
       , (18, 210.5), (20, 208.8)]

file3 :: [(Int, Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
       , (13, 201.5), (14, 203.5), (17, 210.5)
       , (24, 215.1), (25, 218.7)]

file4 :: [(Int, Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
       , (29, 222.8), (30, 223.8), (31, 221.7)
       , (32, 222.3), (33, 220.8), (34, 219.4)
       , (35, 220.1), (36, 220.6)]

-- We want a parameterized data type in order to be able
-- to work with different data:
-- - sales (Double)
-- - "did we meet the sales goal?" (Bool)
-- - "who was the lead sales person?" (String)

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS allTimes extendedValues where
  allTimes = [minimum times .. maximum times]
  extendedValues = map (\time -> Map.lookup time timeValueMap) allTimes
  timeValueMap = Map.fromList (zip times values)

fileToTS :: [(Int, a)] -> TS a
fileToTS timeValuePairs = createTS times values where
  (times, values) = unzip timeValuePairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat[show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat[show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows where
    rows = zipWith showTVPair times values

-- Files converted to TS
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- Combining
combineTSver1 :: TS a -> TS a -> TS a
combineTSver1 x (TS [] []) = x
combineTSver1 (TS [] []) x = x
combineTSver1 (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues where
  bothTimes = mconcat [t1, t2]
  completeTimes = [minimum bothTimes .. maximum bothTimes]

  combinedValues = map (\time -> 
    if isJust (v2Lookup time) then 
      fromJust (v2Lookup time) 
    else if isJust (v1Lookup time) then
      fromJust (v1Lookup time)
    else 
      Nothing
    ) completeTimes 

  firstMap = Map.fromList (zip t1 v1)
  secondMap = Map.fromList (zip t2 v2)

  v1Lookup time = Map.lookup time firstMap
  v2Lookup time = Map.lookup time secondMap 

-- Better version

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v 
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (x, Just y) = Map.insert x y myMap

combineTS :: TS a -> TS a -> TS a
combineTS x (TS [] []) = x
combineTS (TS [] []) x = x
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues where
  bothTimes = mconcat [t1, t2]
  completeTimes = [minimum bothTimes .. maximum bothTimes]

  tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
  updatedMap = foldl insertMaybePair tvMap (zip t2 v2)

  combinedValues = map (\time -> Map.lookup time updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-- Stats

-- Mean

mean :: Real a => [a] -> Double 
mean xs = (realToFrac . sum) xs / (realToFrac . length) xs

meanTS :: Real a => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
  where avg = mean (map fromJust (filter isJust values))                           

-- Min Max
type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = newFunc where
  newFunc (_, Nothing) (t2, maybeY) = (t2, maybeY)
  newFunc (t1, maybeX) (_, Nothing) = (t1, maybeX)
  newFunc (t1, Just x) (t2, Just y) = if f x y == x then (t1, Just x) else (t2, Just y)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values then Nothing
                               else Just best
  where best = foldl compareFunc (0, Nothing) pairs 
        compareFunc = makeTSCompare func
        pairs = zip times values

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max 

minOfAll = minTS tsAll
maxOfAll = maxTS tsAll

-- Diffing

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing = Nothing
diffPair Nothing _ = Nothing
diffPair (Just x) (Just y) = Just (x - y)


diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues) where
  diffValues = zipWith diffPair (tail values) values

meanChange = meanTS (diffTS tsAll)

-- Smoothing

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if any (== Nothing) vals
                 then Nothing
                 else (Just avg)
  where avg = mean (map fromJust vals) 

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] _ = []
movingAvg vals n = if length slice == n
                   then meanMaybe slice : (movingAvg (tail vals) n)
                   else []
  where
  slice = take n vals 

ex = movingAvg [Just 1, Just 2, Just 3, Just 4, Just 3, Just 2] 3

-- Pad first
-- Then compute averages on padded data
-- This causes:
-- Early and late windows to include Nothing
-- meanMaybe returns Nothing
-- Valid averages get dropped entirely, not just shifted
-- ➡️ The output is shorter and more sparse than necessary
-- so, this version is worse
movingAverageTSVer0 :: (Real a) => TS a -> Int -> TS Double
movingAverageTSVer0 (TS [] []) _ = TS [] []
movingAverageTSVer0 (TS times values) n = TS times smoothedValues
  where smoothedValues = movingAvg centered n
        centered = mconcat [nothings, values, nothings] 
        nothings = replicate (n `div` 2) Nothing
        
-- Compute all valid averages
-- Then shift them into the correct time slots
-- This preserves:
-- all valid averages
-- correct alignment
-- original time axis length
-- this is the correct approach
movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where ma = movingAvg values n
        smoothedValues = mconcat [nothings, ma, nothings] 
        nothings = replicate (n `div` 2) Nothing

exTS = (TS [1, 2, 3, 4] [Just 1, Nothing, Just 3, Just 4])
n = 3
worse = movingAverageTSVer0 exTS n
better = movingAverageTS exTS n

{- In this case worse == better.
 The real difference appears as soon as you slightly relax the rules.

Your meanMaybe is very strict:

any Nothing ⇒ Nothing

That’s pedagogical, not realistic.

 A realistic moving average (this is the real example you asked for)

In real time-series work, we usually say:

Compute the average of the values that exist.

Let’s change only this:

meanMaybeLoose :: Real a => [Maybe a] -> Maybe Double
meanMaybeLoose xs =
  case catMaybes xs of
    [] -> Nothing
    ys -> Just (mean ys)


Now redefine:

movingAvgLoose :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingAvgLoose vals n =
  if length slice == n
  then meanMaybeLoose slice : movingAvgLoose (tail vals) n
  else []
  where slice = take n vals

Now the diff becomes visible:

exTS = TS [1,2,3,4,5] [Just 1, Nothing, Just 3, Just 4, Just 5]
n = 3
-}


-- Homework 1: create a function that calculates the div rather than the diff of data,
-- capturing the percent change


divPair :: Real a => Maybe a -> Maybe a -> Maybe Double 
divPair _ Nothing = Nothing
divPair Nothing _ = Nothing
divPair (Just x) (Just y) = Just (realToFrac x / realToFrac y)


divTS :: Real a => TS a -> TS Double
divTS (TS [] []) = TS [] []
divTS (TS times values) = TS times (Nothing:divValues) where
  divValues = zipWith divPair (tail values) values

percentChange = divTS tsAll

-- Homework 2: standard deviation

sqrMaybe :: Num a => Maybe a -> Maybe a 
sqrMaybe Nothing = Nothing 
sqrMaybe (Just x) = Just (x ^ 2)

diffMean :: Real a => Maybe a -> Maybe Double -> Maybe Double
diffMean Nothing _ = Nothing
diffMean _ Nothing = Nothing
diffMean (Just x) (Just m) = Just (realToFrac x - m)

sumMaybe :: Num a => [Maybe a] -> a
sumMaybe xs = sum (map (\(Just x) -> x) (filter isJust xs))

standardDeviation :: Real a => TS a -> Maybe Double
standardDeviation (TS _ []) = Nothing 
standardDeviation (TS times values) = Just (sqrt variance)
  where mean = meanTS (TS times values)
        variance = sumDiff / n 
        sumDiff = realToFrac (sumMaybe (map (\x -> sqrMaybe(diffMean x mean)) values))
        n = realToFrac (length (filter isJust values))

sd = standardDeviation tsAll





























