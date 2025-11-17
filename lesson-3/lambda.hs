test = (\x y z -> (x + y + z) ^ 2 - (x ^ 2 + y ^ 2 + z ^ 2)) 4 10 22

calcChangeLambda owed given =
  ( \change ->
      if change > 0
        then change
        else 0
  )
    (owed - given)

sumSquareOrSquareSum x y =
  ( \sumSquare squareSum ->
      if sumSquare > squareSum
        then sumSquare
        else squareSum
  )
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

doubleDouble x = dubs * 2
  where
    dubs = x * 2

ddl x = (\dubs -> dubs * 2) (x * 2)

sSOSSL x y =
  let sumSquare = (x ^ 2 + y ^ 2)
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum

overwrite x =
  let x = 2
   in let x = 3
       in let x = 4
           in x

overwriteLambda x = (\x -> (\x -> (\x -> x) 4) 3) 2

counterWrong x =
  let x = x + 1
   in let x = x + 1
       in x

counter x = (\x -> (\x -> x + 1) x + 1) x

counterN x =
  (\x -> x + 1)
    ( (\x -> x + 1)
        ( (\x -> x)
            x
        )
    )