-- Readable Show for lists
data List a = Empty | Colon a (List a) deriving (Read, Eq)

instance Foldable List where
  foldr f x Empty = x
  foldr f x (Colon y ys) = f y (foldr f x ys)

instance Functor List where
  fmap f Empty        = Empty
  fmap f (Colon x xs) = Colon (f x) (fmap f xs)

-- Here's the readable Show instance:
instance (Show a) => Show (List a) where
  show Empty        = "<>"
  show (Colon x xs) = "<" ++ (show x) ++ (showI xs) ++ ">" where
    -- Auxiliary function ("show inside")
    showI :: (Show a) => (List a) -> String
    showI Empty        = ""
    showI (Colon x xs) = "," ++ (show x) ++ (showI xs)

{- Witnesses that the derived ordering of `List a` isn't a well-ordering
(by generating the sequence "T > FT > FFT > FFFT > …") — note that it won't
work with the explicit instantiation of Ord. -}
boolDesc :: [(List Bool)]
boolDesc = [a | a <- (map (boolAux) [0..])] where
  boolAux :: Int -> (List Bool)
  boolAux 0 = Colon True Empty
  boolAux n = Colon False (boolAux (n-1))

-- Well-ordering instantiation for List
instance (Ord a) => Ord (List a) where
  x < Empty                      = False
  Empty < x                      = True
  (Colon x xs) < (Colon y Empty) = if xs == Empty then x < y else False
  (Colon x Empty) < (Colon y ys) = if ys == Empty then x < y else True
  (Colon x xs) < (Colon y ys)    = if x == y then xs < ys else x < y
  x > y                          = y < x
  x <= y                         = x < y || x == y
  x >= y                         = y <= x
  compare x y
    | x < y                      = LT
    | x == y                     = EQ
    | x > y                      = GT

-- Instantiating Foldable for Tree
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
  fmap f (Leaf x)       = Leaf (f x)
  fmap f (Node x t1 t2) = Node (f x) (fmap f t1) (fmap f t2)

-- I looked this up on the internet after a lot of failed attempts, sorry :(
instance Foldable Tree where
  foldr f x (Leaf y)       = f y x
  foldr f x (Node y t1 t2) = foldr f (f y (foldr f x t2)) t1
