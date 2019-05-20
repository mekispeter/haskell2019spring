{-
  Functional Programming for Logicians 2019 Spring
  May 6 session
  Sections:
  1 Instantiating Monad for List
  2 Instantiating Monad for Maybe
  3 Monadic operations vs do notation
  4 Monadizing list comprehension
  We started with a considerable amount of initial code in each section.
-}

{-
  1.1 Instantiating Monad for List: initial code
-}

-- These definitions will be familiar from previous sessions:

data List a = Empty | Col a (List a) deriving Eq

lplus :: List a -> List a -> List a
lplus Empty ys          = ys
lplus (Col x xs) ys     = Col x (lplus xs ys)

instance Show a => Show (List a) where
  show Empty            = "<>"
  show (Col x Empty)    = "<" ++ show x ++ ">"
  show (Col x xs)       = "<" ++ show x ++ "," ++ tail (show xs)

instance Functor List where
  fmap f Empty          = Empty
  fmap f (Col x xs)     = Col (f x) (fmap f xs)

instance Applicative List where
  pure x                = Col x Empty
  Empty <*> xs          = Empty
  (Col f fs) <*> xs     = fmap f xs `lplus` (fs <*> xs)

lstx :: List Integer
lstx = Col 1 $ Col 2 $ Col 3 $ Col 4 $ Col 5 $ Col 6 $ Col 7 Empty
lstf :: List (Integer -> Integer)
lstf = Col (^2) $ Col (^3) $ Col (^4) Empty

lx :: [Integer]
lx = [1, 2, 3, 4, 5, 6, 7]
lf :: [Integer -> Integer]
lf = [(^2), (^3), (^4)]
-- > lf <*> lx
-- [1,4,9,16,25,36,49,1,8,27,64,125,216,343,1,16,81,256,625,1296,2401]
-- > lstf <*> lstx
-- <1,4,9,16,25,36,49,1,8,27,64,125,216,343,1,16,81,256,625,1296,2401>

{-
  1.2 Instantiating Monad for List: new code
-}


-- Example on the whiteboard:
-- xs :: Integer
-- xs = [1,2,3]
-- f :: Integer -> [Integer]
-- f 1 = [1]
-- f 2 = [2,2]
-- f 3 = [3,3,3]
-- xs >>= f = [1,2,2,3,3,3]


-- First we instantiate the Monad class with the whiteboard example in mind:
instance Monad List where
  -- return :: a -> m a
  return = pure
  -- >>= binding
  -- (>>=) :: m a -> (a -> m b) -> m b
  Empty >>= f           = Empty
  Col x xs >>= f        = f x `lplus` (xs >>= f)
  -- >> sequencing
  -- (>>) :: m a -> m b -> m b
  (>>) = undefined
  -- We didn't define sequencing because this operation doesn't make much sense
  -- until we start using it in an action context like IO.


-- We work out the example both for our own List type and the built-in [] list
-- type to make sure our instantiation of >>= is the same as the standard one.


enumeratorL :: Integer -> List Integer
enumeratorL n = enumAuxL n 0 where
  enumAuxL n m
    | m < n = Col n (enumAuxL n (m+1))
    | otherwise = Empty

enumerator :: Integer -> [Integer]
enumerator n = enumAux n 0 where
  enumAux n m
    | m < n = n : (enumAux n (m+1))
    | otherwise = []

-- This function will check whether a List list and a [] list contain the same
-- values in the same order.
eqL :: Eq a => List a -> [a] -> Bool
eqL = undefined -- Homework!

{-
  2.1 Instantiating Monad for Maybe: initial code
-}

-- These definitions will be familiar from previous sessions:

data HunMaybe a = Semmi | Csak a deriving (Eq, Show)

instance Functor HunMaybe where
  fmap f Semmi          = Semmi
  fmap f (Csak x)       = Csak $ f x

instance Applicative HunMaybe where
  pure                  = Csak
  Semmi <*> _           = Semmi
  _ <*> Semmi           = Semmi
  Csak f <*> Csak x     = Csak $ f x

{-
  Instantiating Monad for Maybe: new code
-}

-- The instantiation of Monad for Maybe is almost obvious:

instance Monad HunMaybe where
  -- return :: a -> m a
  return  = pure
  -- >>= binding
  -- (>>=) :: m a -> (a -> m b) -> m b
  Csak x >>= f = f x
  Semmi >>= f = Semmi
  -- >> sequencing
  -- (>>) :: m a -> m b -> m b
  (>>) = undefined
  -- Just like with the List type, sequencing doesn't make much sense in the
  -- context of Maybe, so we skipped it.

-- Now an example that we will use in binding: a variant of factorial that
-- handles negative values without raising an error message.

factorialHM :: Integer -> HunMaybe Integer
factorialHM n
  | n < 0 = Semmi -- no useful value is returned
  | otherwise = Csak $ factorial n where -- a useful value is returned
    factorial 0 = 1
    factorial n = n * factorial (n-1)

factorialM :: Integer -> Maybe Integer
factorialM n
  | n < 0 = Nothing
  | otherwise = Just $ factorial n where
    factorial 0 = 1
    factorial n = n * factorial (n-1)

{-
  Monadic operations vs do notation: initial code
-}

-- Two pieces of code we made last week with do notation:

greetingIO :: IO ()
greetingIO = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)

factIO :: IO()
factIO = do
  putStrLn "Factorial of what?"
  num <- getLine
  let fact = factorial $ read num :: Integer
  putStrLn $ show fact
  where
    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial n = n * factorial (n-1)

{-
  Monadic operations vs do notation: new code
-}

-- Do notation is just syntactic sugar for monadic operations:
-- x <- ym
-- --where x is a variable and ym is data in monadic context--desugars to
-- ym >>= \x -> ...
-- The lambda abstraction converts the rest of the do block into a function
-- required by >>=.
-- x
-- y
-- --where x and y are expressions separated by a line break--desugars to
-- x >> y

-- Now the first one is simple:
greetingIO' :: IO ()
greetingIO' =
  (putStrLn "What's your name?") >>
  (getLine >>= \name -> putStrLn ("Hello, " ++ name))

-- The second one will be almost as simple as the first, except it a call for a
-- pure function. This is left for homework.
factIO' :: IO()
factIO' = undefined -- Homework!

{-
  Monadizing list comprehansion: initial code
-}

-- We start with two list comprehensions.
-- The first one lists squares up to 10:
sq :: [Integer]
sq = [x^2 | x <- [1..10]]
-- The second one lists irreducible Pythagorean triples up to 100:
pyth3 :: [(Integer, Integer, Integer)]
pyth3 = [(x,y,z) | x <- [1..100],
                   y <- [x..100],
                   z <- [y..100],
                   x^2 + y^2 == z^2,
                   gcd x y == 1]

{-
  Monadizing list comprehansion: new code
-}
-- Monadic desugaring of the first list comprehension:
sq' :: [Integer]
sq' = [1..10] >>= \x -> return (x^2)
-- A simpler desugaring the first list comprehension without monads:
sq'' :: [Integer]
sq'' = map (^2) [1..10]

-- This will be slightly more complex than the first one. Keep in mind that
-- lambdas can be embedded (... (\x -> ... (\y -> ...))); and that an
-- 'if cond then val1 else val2' expression has the same type as val1 and val2.
pyth3' = undefined
-- Desugaring with map and filter will be even simpler:
pyth3'' = undefined -- Homework!

-- Now we generalize the above desugaring techniques by defining generic
-- functions that desugar any list comprehension of the form
-- [oper x | x <- base, cond x]

-- Monadic version:
compr :: (b -> a) -> [b] -> (b -> Bool) -> [a]
compr oper base cond = undefined -- Homework!

-- Version with map and filter:
compr' :: (b -> a) -> [b] -> (b -> Bool) -> [a]
compr' operation base condition = undefined -- Homework!
