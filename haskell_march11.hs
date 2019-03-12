{-
  Functional Programming for Logicians
  2019 March 11
-}

{-
  Variations on the Fibonacci algorithm. The functions below return the
  nth Fibonacci number, according to the following definition:
  Fib 0 = 0
  Fib 1 = 1
  Fib n = Fib (n-1) + Fib (n-2)
-}

{-
  This is a straightforward implementation. Since recursion is bifurcating (at
  each recursive step there are two recursive calls, at the kth step there are
  2^k calls), execution time grows exponentially.
-}
fibDummy :: Integer -> Integer
fibDummy 0 = 0
fibDummy 1 = 1
fibDummy n = fibDummy (n-1) + fibDummy (n-2)

{-
  A faster version, nut not very memory efficient. Now we keep track of the
  whole list, which exists in the memory in many copies during recursion,
  occupying a lot of ram.
-}
fibFromList :: Integer -> Integer
fibFromList n = head (fibList n) where
  fibList :: Integer -> [Integer]
  fibList 0 = [0]
  fibList 1 = [1,0]
  fibList n = (head oldList + head (tail oldList)) : oldList where
    oldList = fibList (n-1)

{-
  This is a smarter version. We don't need the whole list, only its first two
  members. So why would we keep the rest?
-}
fibFromLSmart :: Integer -> Integer
fibFromLSmart n = head (fibListS n) where
  fibListS :: Integer -> [Integer]
  fibListS 0 = [0]
  fibListS 1 = [1,0]
  fibListS n = (head oldList + head (tail oldList)) : [head oldList] where
    oldList = fibListS (n-1)

{-
  A variation of the previous one: we don't need a list at all. We can pass
  the extra number as another argument. n is now counting backwards.
-}
fibonacciSmart :: Integer -> Integer
fibonacciSmart n = fibAux n 0 1 where
  fibAux :: Integer -> Integer ->
            Integer -> Integer
  fibAux 0 x y = y
  fibAux n x y = fibAux (n-1) y (x+y)

{-
  Or we can pass a pair; yet another way to go.
-}
fibSmart2 :: Integer -> Integer
fibSmart2 n = snd (fibAux2 n) where
  fibAux2 :: Integer -> (Integer,Integer)
  fibAux2 1 = (0,1)
  fibAux2 n = (second, first + second) where
    (first,second) = fibAux2 (n-1)

{-
  Types and type classes: introduction
-}

{-
The following type definitions are mere abbreviations:
-}
-- Relation of integers
type IntRel = Integer -> Integer -> Bool
-- Property of integers
type IntProp = Integer -> Bool
-- Relation of characters
type CharRel = Char -> Char -> Bool
-- Property of characters
type CharProp = Char -> Bool

{-
  This is a general pattern, applicable to every type. It deserves a general
  definition.
-}
-- Relation of values of any type
type Relation a = a -> a -> Bool
-- Property of relation sof any type
type Property a = a -> Bool

{-
  Now we can go for general definitions of doing stuff with relations.
-}
-- Flipping the arguments of a relation, no matter what type:
flipArg :: Relation a -> Relation a
(flipArg r) x y = r y x

{-
  Let's reconsider an old example.
-}
-- Palindrom checking for strings
isPalS :: Property String
isPalS ""              = True
isPalS (c:"")          = True
isPalS s
  | head s == last s  = isPalS (init (tail s))
  | otherwise         = False
--Palindrom checking for lists of numbers
isPalI :: Property [Integer]
isPalI []              = True
isPalI (c:[])          = True
isPalI s
  | head s == last s  = isPalI (init (tail s))
  | otherwise         = False
{-
  The general version raises an error unless we restrict the type variable a
  to types that belong to the Eq class. The reason is that '==' is not
  applicable to every type. Eg. equality of functions is in general undecidable
  (cf. halting problem, Church-Turing theorem).
-}
-- Palindrome checking for any instance of the Eq class:
isPal :: (Eq a) => Property [a]
isPal []              = True
isPal (c:[])          = True
isPal s
  | head s == last s  = isPal (init (tail s))
  | otherwise         = False

{-
  Now we implement the Quicksort algorithm. It orders lists very efficiently
  following these instructions:
  - Pick the first element of the list! (Or any other, but in Haskell the head
    is always a good pick.)
  - Collect the elements that are smaller than the picked element in one list
    called Lower, and the ones that are larger or equal in another list called
    Higher.
  - Repeat the process with Lower and Higher.
  The recursion is bifurcating, just like in the dummy Fibonacci implementation.
  In this case it is inevitable. Sorting is a slow process.
-}
-- Quicksorting integer lists:
quickSortI :: [Integer] -> [Integer]
quickSortI []    = []
quickSortI (n:s) = quickSortI lower ++ [n]
                  ++ quickSortI higher where
                    lower  = filter (<n) s
                    higher = filter (n<=) s
{-
  The general version raises an error unless we restrict the type variable to
  the Ord class. The reason is that some types are not orderable; '<' and '<='
  are not applicable to data of those types. Eg. What would be an ordering of
  functions of type IntRel?
-}
-- Quicksorting lists of values of any orderable type:
quickSort :: (Ord a) => [a] -> [a]
quickSort []    = []
quickSort (n:s) = quickSort lower ++ [n]
                  ++ quickSort higher where
                    lower  = filter (<n) s
                    higher = filter (n<=) s
