import Data.Char

myMap f [] = []
myMap f (x : xs) = f x : myMap f xs

addAnA [] = []
addAnA (x : xs) = ("a " ++ x) : addAnA xs

squareAll [] = []
squareAll (x : xs) = (x ^ 2) : squareAll xs

myFilter test [] = []
myFilter test (x : xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

myRemove test [] = []
myRemove test (x : xs) =
  if test x
    then myRemove test xs
    else x : myRemove test xs

myProduct aList = foldl (*) 1 aList

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

rcons x y = y : x

myReverse xs = foldl rcons [] xs

myFoldl f start [] = start
myFoldl f start (x : xs) = myFoldl f (f start x) xs

myFoldR f start [] = start
myFoldR f start (x : xs) = f x (myFoldR f init xs)

-- Homework
myElem x xs = length (filter (\y -> y == x) xs) > 0

isPalindrome xs = cleaned == reverse cleaned
  where
    cleaned = map toLower (filter (/= ' ') xs)

harmonic n = foldl (+) 0 (map (1 /) [1 .. n])

harmonicBook n = sum (take n seriesValues)
  where
    seriesPairs = zip (cycle [1.0]) [1.0, 2.0 ..]
    seriesValues = map (\pair -> (fst pair) / (snd pair)) seriesPairs

{-
Both versions use lazy evaluation! Haskell is lazy by default.

Your version:

- [1..n] is generated lazily
- map (1 /) transforms elements lazily as needed
- However, foldl is strict in practice (forces evaluation of the whole list to compute the sum)

Book's version:

Creates an infinite list seriesValues (never terminates)
take n extracts only first n elements (lazy evaluation essential here!)
sum then forces evaluation of those n elements
The key difference: the book's version demonstrates working with an infinite list and using take to limit it - showcasing laziness more explicitly. Your version works with a finite list [1..n], so while it still uses lazy evaluation, it's less dramatic of a demonstration.

Both are equally lazy, but the book's version better demonstrates the power of lazy evaluation by starting with infinity and taking only what's needed.
-}