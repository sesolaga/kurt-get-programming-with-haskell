simple x = x

x = 1

calcChangeBad owed given = if owed - given > 0
                        then owed - given
                        else 0

calcChangeBetter owed given = if change > 0
                              then change
                              else 0
                              where change = owed - given

inc n = n + 1

double n = n * 2

square n = n * n

q23 n = if n `mod` 2 == 0
        then n - 2
        else 3 * n + 1