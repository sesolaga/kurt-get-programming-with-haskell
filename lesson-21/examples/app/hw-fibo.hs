fiboSlow :: Int -> Int
fiboSlow 0 = 0
fiboSlow 1 = 1
fiboSlow n = fiboSlow (n - 1) + fiboSlow (n - 2)

fiboFast :: Int -> Int -> Int -> Int
fiboFast _ _ 0 = 0
fiboFast _ _ 1 = 1
fiboFast _ _ 2 = 1
fiboFast m n 3 = m + n 
fiboFast m n x = fiboFast (m + n) m (x - 1)

fibo :: Int -> Int
fibo = fiboFast 1 1

fiboMain :: IO Int
fiboMain = do
  putStrLn "Enter a whole number:"
  n <- getLine
  return (fibo (read n))
