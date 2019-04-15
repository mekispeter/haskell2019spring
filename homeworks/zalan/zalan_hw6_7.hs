-- sorry, if i'll have more time, definitely i'll do the harder stuffs.

mySum :: (Num a) => [a] -> a
mySum lst  = (foldr (+) 0 lst)

myProduct :: (Num a) => [a] -> a
myProduct lst = foldr (*) 1 lst

myMaximum :: (Ord a) => [a] -> a
myMaximum lst = foldr (max) (head lst) lst

squareSum :: (Num a) => [a] -> a
squareSum lst = foldr (+) 0 (map (^2) lst)

-- foldr is used implicitly through myMaximum, i know it's cheating
myLength :: [a] -> Int
myLength lst = myMaximum len where
  len = map (snd) (zip lst numbers) where
    numbers :: [Int]
    numbers = [1..]

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact a = foldl (*) a list where
  list :: [Int]
  list = [1..(a-1)]

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)


treelength :: Tree a -> Int
treelength (Leaf x) = 1
treelength (Node x t1 t2) = 1 + (treelength t1) + (treelength t2)

depth :: Tree a -> Int
depth (Leaf x) = 1
depth (Node x t1 t2) = if treelength t1 < treelength t2 then 1 + (depth t2) else 1 + (depth t1)

occurAux :: (Eq a) => a -> Tree a -> [Bool]
occurAux n (Leaf x) = [n==x]
occurAux n (Node x l r)
  | n == x = [True]
  | otherwise  = (occurAux n l) ++ (occurAux n r)

occurs :: (Eq a) => a -> Tree a -> Bool
occurs n tree
  | elem True (occurAux n tree) = True
  | otherwise = False

treeFlip :: Tree a -> Tree a
treeFlip (Leaf a) = Leaf a
treeFlip (Node a l r) = (Node a (treeFlip r) (treeFlip l))

data Set a = Set [a]

instance Functor Set where
  fmap f (Set []) = Set []
  fmap f (Set (x:xs)) = Set (f x : (fmap f xs))

subset :: (Eq a) => Set a -> Set a -> Bool
subset (Set []) (Set set) = True
subset (Set set1) (Set set2)
  | elem (head set1) set2 = subset (Set (tail set1)) (Set set2)
  | otherwise = False

instance (Eq a) => Eq (Set a) where
  (==) (Set set1) (Set set2) = (subset (Set set1) (Set set2)) && (subset (Set set2) (Set set1))

occur :: (Eq a, Show a) => [a] -> String
occur lst
  | lst == [] = ""
  | elem (head lst) (tail lst) = occur (tail lst)
  | tail lst /= [] = show (head lst) ++ "," ++ (occur (tail lst))
  | otherwise = show (head lst) ++ (occur (tail lst))


instance (Show a, Eq a) => Show (Set a) where
  show (Set a) =  "{" ++ (occur a) ++ "}"

element :: (Eq a) => a -> Set a -> Bool
element a (Set lst) = elem a lst

elementlist :: (Eq a, Show a) => Set a -> [a]
elementlist (Set a) = list a where
  list :: (Eq a, Show a) => [a] -> [a]
  list a
    | a == [] = []
    | elem (head a) (tail a) = list (tail a)
    | otherwise = [(head a)] ++ (list (tail a))

union :: (Eq a, Show a) => Set a -> Set a -> Set a
union (Set set1) (Set  set2) = Set (elementlist (Set set1) ++ (elementlist (Set set2)))

inter :: (Eq a, Show a) => Set a -> Set a -> Set a
inter (Set set1) (Set  set2) = Set (aux (elementlist (Set set1)) (elementlist (Set set2))) where
  aux :: (Eq a) => [a] -> [a] -> [a]
  aux s1 s2
    | s1 == [] = []
    | elem (head s1) s2 = [head s1] ++ (aux (tail s1) s2)
    | otherwise = aux (tail s1) s2

data List a = Empty | Colon a (List a) deriving  (Read, Eq, Ord)

list :: (Eq a, Show a) => List a -> String
list (Colon a lst)
  | lst == Empty = (show a)
  | otherwise = (show a) ++ "," ++ (list lst)
list Empty = undefined

instance (Show a, Eq a) => Show (List a)  where
  show (Colon a lst) = "<" ++ (list (Colon a lst)) ++ ">"
  show Empty = "<>"

shift1 :: [a] -> List a
shift1 [] = Empty
shift1 (x:xs) = Colon x (shift1 xs)

shift2 :: List a -> [a]
shift2 Empty = []
shift2 (Colon a lst) = a : (shift2 lst)

heaD :: List a -> a
heaD lst = (head (shift2 lst))

taiL :: List a -> List a
taiL lst = shift1(tail (shift2 lst))

iniT :: List a -> List a
iniT lst = shift1(init (shift2 lst))

lasT :: List a -> a
lasT lst = (last (shift2 lst))

add :: List a -> List a -> List a
add lst1 lst2 = shift1 ((shift2 lst1) ++ (shift2 lst2))
