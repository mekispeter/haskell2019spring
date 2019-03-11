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
            : take 2 oldlist where
              oldlist = fiblist' (n-1)

fibfromlist' :: Integer -> Integer
fibfromlist' n = head (fiblist' n)
