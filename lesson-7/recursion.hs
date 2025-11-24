myTakeInitial n aList =
  if n > 0
    then
      if length aList > 0
        then head aList : myTakeInitial (n - 1) (tail aList)
        else myTakeInitial (n - 1) []
    else []

myTake n aList =
  if length aList > 0 && n > 0
    then
      head aList : myTake (n - 1) (tail aList)
    else []

-- Greatest Common Divisor (Наибольший общий делитель) gcd 20 16 = 4
gcdMine a b =
  if remainder == 0
    then b
    else gcdMine b remainder
  where
    remainder = a `mod` b

sayAmountCase n = case n of
  1 -> "one"
  2 -> "two"
  n -> "a bunch"

sayAmountPatternMatching 1 = "one"
sayAmountPatternMatching 2 = "two"
sayAmountPatternMatching n = "bunch"

isEmpty [] = True
isEmpty _ = False

myHead (x : _) = x
myHead [] = error "No head for empty list"

myTail (_ : xs) = xs
myTail [] = []

myGcdWithPatternMatching a 0 = a
myGcdWithPatternMatching a b = myGcdWithPatternMatching b (a `mod` b)