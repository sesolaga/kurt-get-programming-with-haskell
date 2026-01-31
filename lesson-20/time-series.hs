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

-- Cobmining
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



































