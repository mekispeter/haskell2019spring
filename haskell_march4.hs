{-
  Functional Programming for Logicians, 2019 March 4.
  These are the functions defined in the fourth session.
  We skipped some of our planned tasks in order to start discussing higher
  order classes as soon as possible.
-}

-- Fibonacci
-- This is a straightforward solution to one of the present homeworks. We
-- observed that the execution time grows exponentially; on the computer used for
-- presentation, 'fibonacci 25' was calculated immediately, 'fibonacci 35' took
-- 11 seconds, and 'fibonacci 45' took more time than we could waste on it. The
-- reason is that the recursion is bifurcating at each step.
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Homework
-- The most challenging homework for next week will be to find a definition that
-- prevents the recursion from bifurcating. The idea is to store the last two
-- values in the sequence at each step, so that there will be no need to
-- calculate the same value twice.
fibFast :: Integer -> Integer
fibFast n = undefined -- this is a mock value so that the code compiles

{-
  Higher-order functions are functions that take functions as arguments, and/or
  return functions.
-}

-- This is our first higher-order function; it takes a unary integer function f,
-- and iterates it twice.
applytwice :: (Integer -> Integer) -> (Integer -> Integer)
applytwice f n = f (f n)

-- The next function is somewhat more general; now the number of iterations is
-- passed as an argument, too. This is our first example of a higher order
-- recursion.
applyntimes :: Integer -> (Integer -> Integer) -> (Integer -> Integer)
applyntimes n f m
  | n == 0        = m
  | otherwise     = f (applyntimes (n-1) f m)

-- This function squares every element of a list.
squareThem :: [Integer] -> [Integer]
squareThem []       = []
squareThem (n:lst)  = (n^2) : squareThem lst

-- This one cubes every element of a list.
cubeThem :: [Integer] -> [Integer]
cubeThem []       = []
cubeThem (n:lst)  = (n^3) : cubeThem lst

-- We discovered a recurring recursive pattern. Now we define a second-order
-- function that will map any unary integer function on the elements of an
-- integer list.
map' :: (Integer -> Integer) -> ([Integer] -> [Integer])
map' f []         = []
map' f (n:lst)    = (f n) : (map' f lst)

-- The abstract definition helps us redefine the previous two functions without
-- explicite recursion (recursion is still there, but it's hidden in the
-- definition of the higher-order function).
squareThem' = map' (^2)
cubeThem'   = map' (^3)

-- This function keeps the even elements of an integer list, and drops the odd
-- ones.
keepEvens :: [Integer] -> [Integer]
keepEvens []      = []
keepEvens (n:lst)
  | even n        = n : (keepEvens lst)
  | otherwise     = keepEvens lst

-- This one does the opposite, using the same recursive pattern.
keepOdds :: [Integer] -> [Integer]
keepOdds []       = []
keepOdds (n:lst)
  | odd n         = n : (keepOdds lst)
  | otherwise     = keepOdds lst

-- We discovered another recursive pattern, and now define a general way to
-- handle it with a higher-order function.
filter' :: (Integer -> Bool) -> ([Integer] -> [Integer])
filter' f []      = []
filter' f (n:lst)
  | f n           = n : (filter' f lst)
  | otherwise     = filter' f lst

-- Now we can define the specific cases without explicite recursion:
keepEvens'  = filter' even
keepOdds'   = filter' odd

-- List comprehension is a very general syntactic tool to manipulate lists. It
-- provides yet another way to handle mapping and filtering:

keepEvens'' :: [Integer] -> [Integer]
keepEvens'' lst = [n | n <- lst, even n]

squareThem'' :: [Integer] -> [Integer]
squareThem lst = [n^2 | n <- lst]

-- Now we can redefine even the map and filter functions without explicite
-- recursion. Recursion is hidden behind list comprehension:
map'' :: (Integer -> Integer) -> ([Integer] -> [Integer])
map'' f lst = [f n | n <- lst]

filter'' :: (Integer -> Bool) -> ([Integer] -> [Integer])
filter'' f lst = [n | n <- lst, f n]

-- We can even define a function that first filters and then maps:
filterAndMap f g lst = [g x | x <- lst, f x]

-- Or one that first maps, and then filters:
mapAndFilter g f lst = [g x | x <- lst, f (g x)]

-- This is yet another general pattern: we apply functions successively. It is
-- called function composition. We define it as yet another higher-order
-- function:
circ ::   (Integer -> Integer) ->
          (Integer -> Integer) ->
          (Integer -> Integer)
circ g f n = g (f n)
