{-
  Functional Programming for Logicians 2019 Spring
  May 6 session
  To Do:
  - Instantiating Monad for List
  - Instantiating Monad for Maybe
  - Monadic operations vs do notation
  - Monadizing list comprehension
  We start with a considerable amount of initial code in each section.
-}

{-
  Instantiating Monad for List: initial code
-}

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
  Instantiating Monad for List: new code
-}

instance Monad List where
  -- return :: a -> m a
  return = undefined
  -- >>= binding
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = undefined
  -- >> sequencing
  -- (>>) :: m a -> m b -> m b
  (>>) = undefined

evenOdd :: Integer -> [String]
evenOdd = undefined

evenOddL :: Integer -> List String
evenOddL = undefined

eqL :: Eq a => List a -> [a] -> Bool
eqL = undefined

{-
  Instantiating Monad for Maybe: initial code
-}

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

instance Monad HunMaybe where
  -- return :: a -> m a
  return  = undefined
  -- >>= binding
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = undefined
  -- >> sequencing
  -- (>>) :: m a -> m b -> m b
  (>>) = undefined

factorialHM :: Integer -> HunMaybe Integer
factorialHM = undefined

{-
  Monadic operations vs do notation: initial code
-}

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

greetingIO' :: IO ()
greetingIO' = undefined

factIO' :: IO()
factIO' = undefined

{-
  Monadizing list comprehansion: initial code
-}

sq :: [Integer]
sq = [x^2 | x <- [1..10]]

pyth3 :: [(Integer, Integer, Integer)]
pyth3 = [(x,y,z) | x <- [1..100],
                   y <- [x..100],
                   z <- [y..100],
                   x^2 + y^2 == z^2,
                   gcd x y == 1]

{-
  Monadizing list comprehansion: new code
-}

sq' = undefined

sq'' = undefined


pyth3' = undefined

pyth3'' = undefined

compr :: (b -> a) -> [b] -> (b -> Bool) -> [a]
compr = undefined

compr' :: (b -> a) -> [b] -> (b -> Bool) -> [a]
compr' = undefined
