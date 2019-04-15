-- Sample
myElem :: (Eq a) => a -> [a] -> Bool
myElem z s  = foldr (isit z) False s where
  isit :: (Eq a) => a -> a -> Bool -> Bool
  isit z x y  = (x == z || y)

-- Péter: Pattern matching is not necessary, since foldr works on an empty
-- list, too. (Note that you don't use m.)
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (m:l)  = foldr (switch) [] (m:l) where
  switch :: a -> [a] -> [a]
  switch c l  = l ++ (c:[])

-- A somewhat simpler version:
myReverse' :: [a] -> [a]
myReverse' l  = foldr (switch) [] l where
  switch :: a -> [a] -> [a]
  switch c l  = l ++ [c]

myLength :: [a] -> Int
myLength []     = 0
myLength (m:l)  = foldr (count) 0 (m:l) where
  count :: a -> Int -> Int
  count c n  = n+1

mySum :: (Num a) => [a] -> a
mySum []     = 0
mySum (m:l)  = foldr (+) 0 (m:l)

-- Explicit instance declarations for HunBool
data HunBool = Hamis | Igaz deriving (Read)

-- Non-exhaustive patterns. You need to add the False case. Uncomment the
-- commented line to avoid an error message for calls like
-- Main> Hamis < Hamis
instance Ord HunBool where
  (<) Hamis Igaz  = True
  -- _ < _ = False
  (<=) x y        = if x == y || x < y then True else False
-- For a minimal complete definition, <= is enough, and < will be generated:
-- Igaz <= Hamis = False
-- _ <= _        = True

-- succ (fromEnum x) = fromEnum (succ x) is expected.
instance Enum HunBool where
  succ Igaz       = Hamis
  succ Hamis      = Igaz
  pred Igaz       = Hamis
  pred Hamis      = Igaz
  fromEnum Igaz   = 1
  fromEnum Hamis  = 0
  toEnum 1        = Igaz
  toEnum 0        = Hamis

instance Bounded HunBool where
  minBound = Hamis
  maxBound = Igaz

instance Eq HunBool where
  (==) Igaz Igaz    = True
  (==) Hamis Hamis  = True
  (==) x y          = False

instance Show HunBool where
  show Igaz   = "Verdad"
  show Hamis  = "Falso"

-- Definition of a "Weekday" type (I couldn't figure out how to define Read)
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday |
  Sunday deriving Read

instance Show Weekday where
  show Monday     = "Lunes"
  show Tuesday    = "Martes"
  show Wednesday  = "Miercoles"
  show Thursday   = "Jueves"
  show Friday     = "Viernes"
  show Saturday   = "Sabado"
  show Sunday     = "Domingo"

instance Eq Weekday where
  (==) Monday Monday        = True
  (==) Tuesday Tuesday      = True
  (==) Wednesday Wednesday  = True
  (==) Thursday Thursday    = True
  (==) Friday Friday        = True
  (==) Saturday Saturday    = True
  (==) Sunday Sunday        = True
  (==) x y                  = False

instance Ord Weekday where
  (<) Monday Tuesday      = True
  (<) Tuesday Wednesday   = True
  (<) Wednesday Thursday  = True
  (<) Thursday Friday     = True
  (<) Friday Saturday     = True
  (<) Saturday Sunday     = True
  (<) Sunday Monday       = False
  (<=) x y                = if x == y || x < y then True else False

instance Enum Weekday where
  succ Monday     = Tuesday
  succ Tuesday    = Wednesday
  succ Wednesday  = Thursday
  succ Thursday   = Friday
  succ Friday     = Saturday
  succ Saturday   = Sunday
  succ Sunday     = Monday
  fromEnum Monday     = 1
  fromEnum Tuesday    = 2
  fromEnum Wednesday  = 3
  fromEnum Thursday   = 4
  fromEnum Friday     = 5
  fromEnum Saturday   = 6
  fromEnum Sunday     = 7
  toEnum 1  = Monday
  toEnum 2  = Tuesday
  toEnum 3  = Wednesday
  toEnum 4  = Thursday
  toEnum 5  = Friday
  toEnum 6  = Saturday
  toEnum 7  = Sunday

instance Bounded Weekday where
  minBound  = Monday
  maxBound  = Sunday

-- Definition of a depth function for the Tree type (last function)
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

mgueTree :: Tree String
mgueTree = Node "S4"
           (Leaf "John")
           (Node "S5" (Leaf "love") (Leaf "Mary"))

numTree :: Tree Integer
numTree = Node 23
          (Leaf 3)
          (Node 20 (Leaf 4) (Leaf 5))

treeLength :: Tree a -> Integer
treeLength (Leaf x)        = 1
treeLength (Node x t1 t2)  = 1 + treeLength t1 + treeLength t2

treeDepth :: Tree a -> Integer
treeDepth (Leaf x)        = 1
treeDepth (Node x t1 t2)  = 1 + (max (treeDepth t1) (treeDepth t2))
