myDrop 0 aList = aList
myDrop n aList = if length aList > 0 then myDrop (n - 1) (tail aList) else myDrop (n - 1) []

myLength [] = 0
myLength aList = 1 + myLength (tail aList)

myLengthWithPatternMatching [] = 0
myLengthWithPatternMatching (_ : xs) = 1 + myLengthWithPatternMatching xs

myTake _ [] = []
myTake 0 _ = []
myTake n aList = head aList : (myTake (n - 1) (tail aList))

myTake2 _ [] = []
myTake2 0 _ = []
myTake2 n (x : xs) = x : rest
  where
    rest = myTake2 (n - 1) xs

myCycle xs = xs ++ myCycle xs

booksCycle (first : rest) = first : booksCycle (rest ++ [first])

{-
These functions are functionally equivalent - they both create an infinite cycle of the input list, but they use different implementation strategies:

Your myCycle:

Approach: Appends the entire list to itself recursively
Pattern: Takes the whole list xs, concatenates it with a recursive call to myCycle xs
Example: myCycle [1,2,3] → [1,2,3] ++ myCycle [1,2,3] → [1,2,3,1,2,3,1,2,3,...]

Book's booksCycle:

Approach: Rotates the list element by element
Pattern: Takes the first element, puts it at the front, and recursively calls with the rotated list (rest + first moved to end)
Example: booksCycle [1,2,3] → 1 : booksCycle [2,3,1] → 1 : 2 : booksCycle [3,1,2] → [1,2,3,1,2,3,...]

Key Differences:
Efficiency: Your version is more efficient as it doesn't need to repeatedly append elements to the end of lists
Memory pattern: Your version works in chunks (repeating the whole list), while the book's version works element by element
Edge cases: The book's version uses pattern matching and assumes a non-empty list, while yours works with any list (though both would have issues with empty lists)
Both produce the same infinite cycling behavior, but your implementation is actually cleaner and more efficient! The book's version demonstrates a different recursive thinking pattern but is less practical.
-}

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

myReverse [] = []
myReverse (x : xs) = rest ++ [x]
  where
    rest = myReverse xs

slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n - 2)

fastFib _ _ 0 = 0 
fastFib _ _ 1 = 1 
fastFib _ _ 2 = 1 
fastFib m n 3 = m + n 
fastFib m n counter = fastFib (m + n) m (counter - 1) 

fib = fastFib 1 1



