ifEven f x =
  if even x
    then f x
    else x

inc x = x + 1

double x = x * 2

square x = x * x

cube x = x ^ 3

ifEvenCube x = ifEven cube x