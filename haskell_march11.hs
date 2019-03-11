{-
  Functional Programming for Logicians
  March 11
-}
fibdummy :: Integer -> Integer
fibdummy 0 = 0
fibdummy 1 = 1
fibdummy n = fibdummy (n-1) + fibdummy (n-2)

fiblist :: Integer -> [Integer]
fiblist 0 = [0]
fiblist 1 = [1,0]
fiblist n = (head oldlist
            + head (tail oldlist))
            : oldlist where
              oldlist = fiblist (n-1)

fibfromlist :: Integer -> Integer
fibfromlist n = head (fiblist n)

fiblist' :: Integer -> [Integer]
fiblist' 0 = [0]
fiblist' 1 = [1,0]
fiblist' n = (head oldlist
            + head (tail oldlist))
            : [head oldlist] where
              oldlist = fiblist' (n-1)

fibfromlist' :: Integer -> Integer
fibfromlist' n = head (fiblist' n)

fibonacciSmart :: Integer -> Integer
fibonacciSmart n = fibAux n 0 1 where
  fibAux :: Integer -> Integer ->
            Integer -> Integer
  fibAux 0 x y = y
  fibAux n x y = fibAux (n-1) y (x+y)
  
