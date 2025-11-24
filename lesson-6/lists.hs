h = head [1, 2, 3] -- 1

t = tail [1, 2, 3] -- [2, 3]

t1 = tail [3] -- []

c1 = 1 : [] -- [1]

c2 = 1 : 2 : 3 : 4 : [] -- [1, 2, 3, 4]

first = [1, 2, 3] !! 0

len = length [1, 2, 3]

rev = reverse "Sergei"

inOrNot = elem 'e' "Solagaian"

isPalindrome word = word == reverse word

t1 = take 3 "Sergei"

askMoreThanItHas = take 10 [1, 2, 3]

takeLast n aList = reverse (take n (reverse aList))

nameShort = reverse (drop 2 (reverse "Sergei"))

z = zip "dog" "rabbit"

alp = zip ['a' .. 'z'] [1 ..]

ones n = take n (cycle [1])

assignToGroups n aList = zip group aList
  where
    group = cycle [1 .. n]

-- Homework
repeatMine x = cycle [x]

dropLast n aList = reverse (drop n (reverse aList))

subseq start end aList = dropLast (length aList - end) (drop start aList)

inFirstHalf x aList = x `elem` (take (length aList `div` 2) aList)

-- Homewor answers

subseqAnswer start end aList = take difference (drop start aList)
  where
    difference = end - start

inFirstHalfAnswer x aList = x `elem` firstHalf
  where
    midpoint = (length aList) `div` 2
    firstHalf = take midpoint aList
