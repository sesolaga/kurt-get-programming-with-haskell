import Data.Map qualified as Map

data Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 5 43

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

reversedPoint = Triple (third aPoint) (second aPoint) (first aPoint)

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

ex1 = transform (* 5) aPoint

-- Lists

data List a = Empty | Cons a (List a) deriving (Show)

builtInEx1 :: [Int]
builtInEx1 = 1 : 2 : 3 : []

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap f Empty = Empty
ourMap f (Cons a rest) = Cons (f a) (ourMap f rest)

ex2 = ourMap (* 2) ourListEx1

-- Tuples

itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

-- Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList (zip ids organs)

valueByKey7 = Map.lookup 7 organCatalog

-- Homework 1

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

ex3 = boxMap (\x -> show x) (Box 2)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

ex4 = tripleMap (\x -> show x) aPoint

-- Homework 2

catalogToList = Map.toList organCatalog

catalogValues = map snd (catalogToList)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map countOrgan allOrgans
  where
    countOrgan = (\organ -> (length . filter (== organ)) catalogValues)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)