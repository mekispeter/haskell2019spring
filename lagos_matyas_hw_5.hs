f1 :: (Eq a) => [a] -> Bool
f1 a = minimum (zipWith (==) a (reverse a))
{-
  f1 is a palindrome checker for lists whose elements can be arguments of the
  "==" function. It returns True if [a] is a palindrome, and False otherwise.
  This is done by reversing the list (with "reverse"), and comparing the ori-
  ginal and the reversed lists' elements pairwise (with "zipWith"). Since this
  results in a list of Booleans, taking its minimum means that we will get False
  if and only if at least one of the elements are false, i.e. if at least one
  pair of elements from the two lists contains non-identical elements.
-}

f2 :: [String] -> Char -> String
f2 a b  = foldl1 (conc b) a where conc b d e = d++(b:e)
{-
  f2 takes a list of strings and the character, and returns a string composed
  of strings in the list with the character concatenated in between. It takes
  the function "c" — which takes a character and two strings and returns a
  string where the character is inserted between the two strings — and "folds"
  it into the list, i.e. it applies it to the first two elements, and then
  applies it to the result of the first application and the third element, and
  so on.
-}

f3 :: Int -> Integer
f3 a = (filter prime [1..]) !! a where
  prime c = all relPrime [1..c-1] where relPrime e = gcd c e == 1
{-
  f3 returns the a-th prime number (if we accept that 1 is the 0-th prime). It
  obtains a list of all primes (and 1) by filtering the list of natural numbers
  by a predicate "b" that checks primeness (returns True iff "c" is prime). "b"
  works by checking whether all of the natural numbers smaller than "c" are
  relative primes of "c" (by checking that their greatest common divisor is 1) —
  if yes, then "c" is an (absolute) prime, if not, then it isn't. Even though
  there is an infinite list "[1..]" in the function, computation is possible
  because it is always indexed by a finite number "a", so Haskell doesn't try
  to compute the entire list of natural numbers.
-}

f4 :: Integer -> [Integer]
f4 a = if a == 0 then [1] else zipWith (+) (0:b) (b++[0]) where
  b = f4 (a-1)
{-
  f4 returns the a-th row of the Pascal triangle. It adds together the neigh-
  bouring members of the previous row with the zipWith functions, by creating
  two versions of the previous row such that when zipped with addition, they
  produce the sums of the neighbouring pairs (by positioning them in the same
  place on each list).
-}

f5 :: Int -> Int
f5 a = b !! a where b = 1:[c * (b !! (c-1)) | c <- [1..]]
{-
  f5 multiplies "a" with the result of its application to "a-1". This is done
  by getting the a-th member of a list "b" that is headed by 1 and contains the
  products of each natural number "c" and the "c-1"-st member of "b". This
  doesn't result in infinite computation because the potentially infinite list
  is always indexed at a finite position.
-}

f6 :: Int -> Integer
f6 a = b !! (a+1) where b = 1:[sum (take c b) | c <- [1..]]

f7 :: Eq a => [a] -> [[a]]
f7 a = if a == [] then [[]] else map (head a:) b ++ b where b = f7 (tail a)

f8 :: Int -> [Int]
f8 a = concat (map b [1..a]) where b a = [1..a]

f9 :: Eq a => [[a]] -> [[a]]
f9 a = if head a == [] then [] else map head a : f9 (map tail a)

f10 :: Int -> Integer
f10 a = b !! a where b = 0 : 1 : [x+y | (x,y) <- zip b (tail b)]

f0 :: Int -> [Int]
f0 a = take a b where b = (1 : [a * c | c <- b])
{-
  f0 a returns the list of the first a powers of a: eg. f0 2 = [1,2];
  f0 5 = [1,5,25,125,625]. The embedded definition of b contains
  self-reference, but it still works. Let's see how. The head of the list, 1,
  is fixed; then the head is used to evaluate the next item 1 * a = a, which is
  then used to evaluate the third, 1 * a * a, and so on. Note that b is an
  infinite list. The whole construction works because Haskell has lazy
  evaluation. Since take takes only a finite portion of b, only a finite
  portion of values is calculated, each of which is determined by the elements
  before it, thus making the evaluation process finite.
-}
