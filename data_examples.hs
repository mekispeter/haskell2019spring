data MyIntList = EmptyI | ColonI Integer MyIntList deriving (Read, Eq, Ord)

instance Show MyIntList where
  show EmptyI = "[[]]"
  show (ColonI x EmptyI) = "[[" ++ (show x) ++ "]]"
  show (ColonI x xs) = take 2 (show xs) ++ (show x) ++ ";" ++ drop 2 (show xs)

list1 :: MyIntList
list1 = ColonI 1 EmptyI

list2 :: MyIntList
list2 = ColonI 1 (ColonI 2 EmptyI)

rangeInt :: Integer -> Integer -> MyIntList
rangeInt n m
  | m <= n    = EmptyI
  | otherwise = ColonI n (rangeInt (n+1) m)


data MyStrTree = LeafS String | NodeS MyStrTree MyStrTree deriving (Read, Eq, Ord)

instance Show MyStrTree where
  show (LeafS s)     = show s
  show (NodeS t1 t2) = "<" ++ (show t1) ++ "," ++ (show t2) ++ ">"

tree1 = NodeS (LeafS "John") (NodeS (LeafS "love") (LeafS "Mary"))

data MyList a = Empty | Colon a (MyList a) deriving (Read, Eq, Ord)

instance (Show a) => Show (MyList a) where
  show Empty = "[[]]"
  show (Colon x Empty) = "[[" ++ (show x) ++ "]]"
  show (Colon x xs) = take 2 (show xs) ++ (show x) ++ ";" ++ drop 2 (show xs)

list3 :: MyList Char
list3 = Colon  'a' Empty

list4 :: MyList Integer
list4 = Colon 1 (Colon 2 Empty)

range :: (Ord a, Enum a) => a -> a -> (MyList a)
range x y
  | y <= x    = Empty
  | otherwise = Colon x (range (succ x) y)

data MyTree a = Leaf a | Node (MyTree a) (MyTree a) deriving (Read, Eq, Ord)

instance (Show a) => Show (MyTree a) where
  show (Leaf x)     = show x
  show (Node t1 t2) = "<" ++ (show t1) ++ "," ++ (show t2) ++ ">"

tree2 :: MyTree String
tree2 = Node (Leaf "John") (Node (Leaf "love") (Leaf "Mary"))

tree3 :: MyTree Integer
tree3 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
