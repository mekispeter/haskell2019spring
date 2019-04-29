-- HW 1
myReverse :: [a] -> [a]
myReverse s = foldr f [] s where f x y = y ++ [x]


-- HW 2
myLength :: [a] -> Int
myLength s = foldr count 0 s where count x y = 1 + y


-- HW 3:
mySum :: (Num a) => [a] -> a
mySum s  = foldr (+) 0 s


-- HW 4
myProduct :: (Num a) => [a] -> a
myProduct s  = foldr (*) 1 s


-- HW 5
myMaximum :: (Ord a) => [a] -> a
myMaximum s = foldr max (head s) s where max x y = if x > y then x else y


-- HW 6
squareSum :: (Num a) => [a] -> a
squareSum s = foldr ss 0 s where ss x y = x^2 + y


-- HW 7
factorial :: (Num a, Enum a) => a -> a
factorial n = foldr f 1 [1..n] where f x y = x*y


data HunBool = Hamis | Igaz deriving (Eq, Show)


-- HW 11
instance Ord HunBool where
  (<) Hamis Igaz  = True
  (<=) x y        = if x == y || x < y then True else False


instance Enum HunBool where
  succ Igaz       = error "bad argument"
  succ Hamis      = Igaz
  pred Hamis      = error "bad argument"
  pred Igaz       = Hamis
  fromEnum Igaz   = 1
  fromEnum Hamis  = 0
  toEnum 1        = Igaz
  toEnum 0        = Hamis


instance Bounded HunBool where
  minBound = Hamis
  maxBound = Igaz


data Tree a = Leaf a | Node a (Tree a) (Tree a)

montagueTree :: Tree String
montagueTree =  Node "S4"
                (Leaf "John")
                (Node "S5" (Leaf "love") (Leaf "Mary"))

montagueTree2 :: Tree String
montagueTree2 =  Node "S4"
                (Leaf "John")
                  (Node "S5"
                  (Leaf "love")
                    (Node "S6" (Leaf "Sunil") (Leaf "Kumar")))

-- HW 13
treeDepth :: Tree a -> Integer
treeDepth (Leaf x)       = 1
treeDepth (Node x t1 t2)
  | treeDepth t1 > treeDepth t2 = 1 + treeDepth t1
  | otherwise = 1 + treeDepth t2


-- HW 14
searchTree :: (Eq a) => a -> Tree a -> Bool
searchTree item (Leaf t) = t == item
searchTree item (Node t t1 t2) = if t == item then True else searchTree item t1 || searchTree item t2


-- HW 15
flipTree :: Tree a -> Tree a
flipTree (Leaf t)       =  (Leaf t)
flipTree (Node t t1 t2) = (Node t (flipTree t2) (flipTree t1))


-- HW 16
branches :: Tree a -> [[a]]
branches (Leaf t) = [[t]]
branches (Node t t1 t2) = map (t :) (branches t1) ++ map (t :) (branches t2)


--HW 17
instance (Show a) => Show (Tree a) where
  show a = showDepth 0 a


showDepth :: (Show a) => Int -> Tree a -> String
showDepth d (Leaf t) = "\n" ++ space d ++ show t
showDepth d (Node t t1 t2) = ("\n" ++ space d ++ show t) ++ (showDepth (d+1) t1) ++ (showDepth (d+1) t2)

space :: Int -> String
space 0 = ""
space d = "-" ++ space (d-1)



-- HW 19
data Tree1 a = Empty1 | Node1 a (Tree1 a) (Tree1 a) deriving Show

montagueTree1 :: Tree1 String
montagueTree1 =  Node1 "S4"
                (Node1 "John" Empty1 Empty1)
                (Node1 "S5" (Node1 "love" Empty1 Empty1) (Node1 "Mary" Empty1 Empty1))
