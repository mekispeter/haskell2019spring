{-
  Functional programming for Logicians, 2019 Spring
  Session 9, April 15
-}

{-
  A quick recap on instantiating the Functor class on the List type from three
  weeks ago before we start discussing the Applicative class:
-}
data List a = Empty | Colon a (List a)
              deriving (Show, Read, Eq, Ord)

instance Functor List where
  fmap f Empty        = Empty
  fmap f (Colon x xs) = Colon (f x) (fmap f xs)

{-
  There are two options to instantiate the Applicative class.
  Iterated mapping
  [^2,^3] <*> [1,2,3] = [1,4,9,1,8,27]
  or
  zipping:
  [^2,^3] <*> [1,2,3] = [1,8]
  We implement the first one, because that's what Haskell's standard list
  type does. For the alternative, see the ZipList type, which is introduced
  for exactly for the purpose of implementing it. (Insert
  "import Control.Applicative" in your code to use it.)
-}

instance Applicative List where
  pure x            = Colon x Empty
  Empty <*> xs      = Empty
  Colon f fs <*> xs = lPlus (fmap f xs) (fs <*> xs)

lPlus :: List a -> List a -> List a
lPlus Empty ys        = ys
lPlus (Colon x xs) ys = Colon x (lPlus xs ys)

{-
  To test how '<*>' works, type 'functions <*> numbers' in ghci:
-}
functions :: List (Integer -> Integer)
functions = Colon (^2) (Colon (^3) Empty)
functions' :: List (Integer -> Integer)
functions' = Colon (^3) (Colon (^4) Empty)
numbers :: List Integer
numbers = Colon 1 (Colon 2 (Colon 3 Empty))

{-
  We test the Applicative laws.
  Identity:
    > pure id <*> numbers
    Colon 1 (Colon 2 (Colon 3 Empty))
  Homomorphism:
    > pure (^3) <*> pure 2 :: List Integer
    Colon 8 Empty
  Interchange:
    > functions <*> pure 2
    Colon 4 (Colon 8 Empty)
    > pure (\f -> f 2) <*> functions
    Colon 4 (Colon 8 Empty)
  Composition:
    > pure (.) <*> functions <*> functions' <*> numbers
    Colon 1 (Colon 64 (Colon 729 (Colon 1 (Colon 256 (Colon 6561 (Colon 1
    (Colon 512 (Colon 19683 (Colon 1 (Colon 4096 (Colon 531441 Empty)))))))))))
    > functions <*> (functions' <*> numbers)
    Colon 1 (Colon 64 (Colon 729 (Colon 1 (Colon 256 (Colon 6561 (Colon 1
    (Colon 512 (Colon 19683 (Colon 1 (Colon 4096 (Colon 531441 Empty)))))))))))
-}

{-
  Another recap from three weeks ago: instantiating the Functor class on the
  HunMaybe type.
-}
data HunMaybe a = Semmi | Csak a
                  deriving (Show, Read, Eq)

instance Functor HunMaybe where
  fmap f Semmi    = Semmi
  fmap f (Csak x) = Csak (f x)

{-
  Instantiating the applicative is similarly simple. We enumerate all cases for
  transparency.
-}
instance Applicative HunMaybe where
  pure x            = Csak x
  Semmi <*> Semmi   = Semmi
  Semmi <*> Csak x  = Semmi
  Csak f <*> Semmi  = Semmi
  Csak f <*> Csak x = Csak (f x)

{-
  Now we test the applicative laws.
  Identity:
    > pure id <*> Semmi
    Semmi
    > pure id <*> Csak "Trump"
    Csak "Trump"
  Homomorphism:
    > Csak head <*> Csak "Trump"
    Csak 'T'
    > Csak (head "Trump")
    Csak 'T'
  Interchange:
    > Csak (++"istan") <*> pure "Trump"
    Csak "Trumpistan"
    > pure (\f -> f "Trump") <*> Csak (++"istan")
    Csak "Trumpistan"
  Composition:
  > pure (.) <*> Csak ("Trump"++) <*> Csak (tail.tail) <*> Csak "Apology"
  Csak "Trumpology"
  > Csak ("Trump"++) <*> (Csak (tail.tail) <*> Csak "Apology")
  Csak "Trumpology"
-}
