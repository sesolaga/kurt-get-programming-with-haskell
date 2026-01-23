howMuch :: Int -> String
howMuch n
  | n > 10 = "a whole bunch"
  | n > 0 = "not much"
  | otherwise = "we are in debt!"

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Clear deriving (Show, Eq)

instance Semigroup Color where
  (<>) Clear x = x
  (<>) x Clear = x
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
    | all (`elem` [Yellow, Red, Orange]) [a, b] = Orange
    | otherwise = Brown

r1 = (Green <> Blue) <> Yellow

r2 = Green <> (Blue <> Yellow)

associativeProof = r1 == r2

-- Probability tables
type Events = [String]

type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  -- my version
  --   show (PTable events probs) = mconcat (map (\(f, s) -> showPair f s) (zip events probs))
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

pt = createPTable ["heads", "tails"] [0.5, 0.5]

-- We need to repeat each element in the first list
-- once for each element in the second
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    -- maps l1 and makes nToAdd copies of each element:
    -- l1 = [1, 2, 3]
    -- nToAdd = 4
    -- repeatedL1 = [[1, 1, 1, 1], [2, 2, 2, 2], [3, 3, 3, 3]]
    repeatedL1 = map (take nToAdd . repeat) l1
    -- newL1 = [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3]
    newL1 = mconcat repeatedL1
    -- l2 = [4, 5, 6, 7]
    -- cycledl2 = [4, 5, 6, 7, 4, 5, 6, 7, 4, 5, 6, 7, ...]
    cycledL2 = cycle l2

ex = cartCombine (+) [1, 2, 3] [4, 5, 6, 7]

-- [5,6,7,8,6,7,8,9,7,8,9,10]

combineEvents :: Events -> Events -> Events
combineEvents events1 events2 = cartCombine (\x y -> mconcat [x, "-", y]) events1 events2

combineProbs :: Probs -> Probs -> Probs
combineProbs probs1 probs2 = cartCombine (*) probs1 probs2

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable1 = ptable1
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable (combineEvents e1 e2) (combineProbs p1 p2)

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

res1 = coin <> spinner

res2 = coin <> coin <> coin

instance Monoid Color where
  mempty = Clear
  mappend = (<>)