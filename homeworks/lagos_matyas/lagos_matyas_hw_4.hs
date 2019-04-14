-- Péter: Very nice job. See the comments on your own functions.

-- Sample (creates a function from a list of pairs)
fnFromPairs :: [(Integer,Integer)] -> (Integer -> Integer)
fnFromPairs [] z         = undefined
fnFromPairs ((x,y):l) z  = if z == x then y else (fnFromPairs l z)

-- Sample (returns the list of the first n primes)
firstPrimes :: Int -> [Integer]
firstPrimes n  = take n [i | i <- [2..], properDivisors i == []] where
    properDivisors :: Integer -> [Integer]
    properDivisors i  = [j | j <- [2..(i-1)], mod i j == 0]

-- Flips the arguments of a binary integer function
flipHw :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
flipHw f n m  = f m n

-- Uncurries a binary function
uncurryHw :: (Integer -> Integer -> Integer) -> ((Integer,Integer) -> Integer)
uncurryHw f (n,m)  = f n m

-- Checks whether a predicate is true of all elements in a list of strings
allHw :: (String -> Bool) -> [String] -> Bool
allHw f []     = True
allHw f (s:l)  = if elem False (map f (s:l)) then False else True
-- The previous function, but with recursion
allHwRec :: (String -> Bool) -> [String] -> Bool
allHwRec f []     = True
allHwRec f (s:l)  = if f s == False then False else allHwRec f l

-- Checks whether a predicate is true of any elements in a list of integers
anyHw :: (Integer -> Bool) -> [Integer] -> Bool
anyHw f []     = False
anyHw f (i:l)  = if f i == True then True else anyHw f l

{-- Zips a string list and an integer list together, so that the resulting list
consists of pairs the first member of which is from the string list, and the
second from the integer list --}
zipHw :: [String] -> [Integer] -> [(String,Integer)]
zipHw l []           = []
zipHw [] l           = []
zipHw (s:sl) (i:il)  = (s,i) : zipHw sl il

-- Péter: Nice idea, but slightly complicated. (Detailed comments and simpler
-- version below.)
-- Checks if a set of integers is a subset of another set of integers
subset :: [Integer] -> [Integer] -> Bool
subset [] y = True
subset x [] = False
subset x y  = if all (cont y) x then True else False
-- Auxiliary for subset (I couldn't figure out how to use flip for elem)
cont :: [Integer] -> Integer -> Bool
cont x y  = elem y x
-- Péter: 1. You can use the infix version: (`elem` y),
-- eg. (`elem` [1,2,3]) 1 == True
-- 2. The first two clauses are redundant.
-- 3. The if-then is redundant.
subset' :: [Integer] -> [Integer] -> Bool
subset' x y = all (`elem` y) x

-- Péter: This is beautiful. See my version below.
-- Composes set-theoretic relations
compose :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
compose f []  = []
compose [] f  = []
compose f g   = map compSmall (compPrep f g)
-- Checks if two ordered pairs can be composed
match :: (Integer,Integer) -> (Integer,Integer) -> Bool
match (a,b) (c,d)  = if b == c then True else False
-- Creates a set of ordered pairs of composable ordered pairs
compPrep :: [(Integer,Integer)] -> [(Integer,Integer)] -> [((Integer,Integer),
    (Integer,Integer))]
compPrep f g = [((a,b),(c,d)) | (a,b) <- f, (c,d) <- g, match (a,b) (c,d)]
-- Composes two ordered pairs
compSmall :: ((Integer,Integer),(Integer,Integer)) -> (Integer,Integer)
compSmall ((a,b),(c,d))  = (a,d)

-- Péter: my version:
compose' :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
compose' f g = [(x,z) | x <- dom f, z <- rng g, connected x z f g] where
  dom :: [(Integer,Integer)] -> [Integer]
  dom f = [x | (x,y) <- f]
  rng :: [(Integer,Integer)] -> [Integer]
  rng f = [y | (x,y) <- f]
  connected :: Integer -> Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
              -> Bool
  connected x z f g = any (connects x z f g) (rng f ++ dom g)
  connects :: Integer -> Integer
              -> [(Integer,Integer)] -> [(Integer,Integer)]
              -> Integer -> Bool
  connects x z f g y = (x,y) `elem` f && (y,z) `elem` g
