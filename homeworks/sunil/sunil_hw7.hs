import Data.Bifunctor

-- HW 1

data List a = Empty | Colon a (List a) deriving (Read, Eq, Ord)

listOne :: List Int
listOne = Colon 1 (Colon 2 (Colon 3 Empty))

listEmpty:: List Int
listEmpty = Empty

instance (Show a) => Show (List a) where
    show Empty = "<>"
    show a     = "<" ++ listify a ++ ">"

listify :: (Show a) => List a -> String
listify (Colon x Empty) = show x
listify (Colon x xs)    = show x ++ "," ++ listify xs


-- HW 5 (partial)

data Tree a b = Leaf a | Node b (Tree a b) (Tree a b) deriving (Show)

myTree :: Tree Int String
myTree = Node "X" (Leaf 1) (Node "Y" (Leaf 2) (Leaf 3))


-- instance Bifunctor Tree where
-- bimap f g (Leaf x) = Leaf (g x)
-- bimap f g (Node x i j) = Node (f x) (bimap f g i) (bimap f g j)


-- HW 8

data Either1 a b = Left1 a | Right1 b

instance (Show a, Show b) => Show (Either1 a b) where 
    show (Left1 a)  = "Left " ++ show a 
    show (Right1 b) = "Right " ++ show b 

instance (Eq a, Eq b) => Eq (Either1 a b) where 
    (==) (Left1 a) (Left1 a1)   = a == a1
    (==) (Right1 b) (Right1 b1) = b == b1
    (==) (Left1 a) (Right1 b)   = False
    (==) (Right1 b) (Left1 a)   = False

instance (Ord a, Ord b) => Ord (Either1 a b) where 
    (<=) (Left1 a) (Left1 a1)   = if a == a1 || a < a1 then True else False
    (<=) (Right1 b) (Right1 b1) = if b == b1 || b < b1 then True else False
    (<) (Left1 a) (Left1 a1)    = a < a1
    (<) (Right1 b) (Right1 b1)  = b < b1
    (<) (Left1 a) (Right1 b)    = True
    (<) (Right1 b) (Left1 a)    = False

{- As mentioned in the description of the Either datatype, it can be used for error handling, 
or more generally, if the desired data is fetchable then it is output using Right and
if it is not fetchable then an appropriate message is output using Left.
-}

-- Example: If number is binary then output the successor, if not then output an error message

checkBinary :: String -> Bool
checkBinary s
    | s == "0" || s == "1"           = True
    | head s /= '0' && head s /= '1' = False
    | otherwise                      = checkBinary (tail s)

bsucc :: String -> String
bsucc s
    | s == "1"           = "10"
    | last s == '0'      = reverse ('1' : tail (reverse s))
    | otherwise          = bsucc(reverse(tail (reverse s))) ++ "0"

bsuccWError :: String -> Either1 String String
bsuccWError b = if checkBinary b then Right1 (bsucc b) else Left1 "Not a binary number!"


{-
    HW 9
    It depends on the use case, in case of error handling, we need not always require that a function maps on the 
    left value, while we might require that the Right value be mappable. For example, in the bsuccWError function,
    I could use a functor to ouput the decimal value of the successor (Right value) while still throwing an 
    error (Left value) when the input is not a binary number.

    Bifunctors are useful for Either when we require both Left and Right to be mappable to different functions.
-}


-- HW 11 

data Set a = Set [a] 

-- Eq instance
instance (Eq a) => Eq (Set a) where
    (==) (Set a) (Set b) = (subSet (Set a) (Set b)) && (subSet (Set b) (Set a))


subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set s1) (Set s2)
    | s1 == []          = True
    | elem (head s1) s2 = subSet (Set (tail s1)) (Set s2)
    | otherwise         = False


-- Show instance
instance (Show a, Eq a) => Show (Set a) where
    show (Set s) = show (reduceSet (Set s))

count :: (Eq a) => a -> [a] -> Int
count e l = length (filter (== e) l)

reduceSet :: (Eq a) => Set a -> [a]
reduceSet (Set s) = reduceSetAux s

reduceSetAux :: (Eq a) => [a] -> [a]
reduceSetAux s
    | count (head s) s >= 1 = head s : reduceSetAux (filter (/= (head s)) s)
    | otherwise             = [t | t <- s , count t s == 1 ]


-- HW 12 

element :: (Eq a) => a -> Set a -> Bool
element e (Set s) = elem e s

elementList :: (Eq a) => Set a -> [a]
elementList s = reduceSet s

setUnion :: (Eq a) => Set a -> Set a -> Set a
setUnion (Set s1) (Set s2) = Set (reduceSet (Set s1) ++ reduceSet (Set s2))

setIntersection :: (Eq a) => Set a -> Set a -> Set a
setIntersection (Set s1) (Set s2) = Set ([i| i <- s1, i <- s2, (elem i s1) && (elem i s2)])


-- HW 13 

instance Functor Set where
    fmap f (Set []) = Set []
    fmap f (Set (x:xs)) = Set (f x : (fmap f xs))
      
