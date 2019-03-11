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

fibSmart2 :: Integer -> Integer
fibSmart2 n = snd (fibAux2 n) where
  fibAux2 :: Integer -> (Integer,Integer)
  fibAux2 1 = (0,1)
  fibAux2 n = (second, first + second) where
    (first,second) = fibAux2 (n-1)

type IntRel = Integer -> Integer -> Bool
type IntProp = Integer -> Bool

type CharRel = Char -> Char -> Bool
type CharProp = Char -> Bool

type Relation a = a -> a -> Bool
type Property a = a -> Bool

flipArg :: Relation a -> Relation a
(flipArg r) x y = r y x

isPal :: (Eq a) => Property [a]
isPal []              = True
isPal (c:[])          = True
isPal s
  | head s == last s  = isPal (init (tail s))
  | otherwise         = False
