{-
  Péter: Good job! See the comments below.
-}

f1 :: (Eq a) => [a] -> Bool
f1 a = minimum (zipWith (==) a (reverse a))
{-
1. Takes any list as input
2. zipWith (==) compares each element of the input list with it's reverse
3. The output list from the previous step contains either true or false
4. minimum function outputs true if all are true and false if either one of them
   is false
5. So f1 checks whether the list is a palindrome or not. Example: [1,2,1] is True
-}


f2 :: [String] -> Char -> String
f2 a b  = foldl1 (c b) a where c b d e = d++b:e
{-
1. Takes a list of strings and a character as input
2. function (c b) takes two strings d and e as parameters
3. function (c b) adds character b to second string e and this (b:e) string is
   concatenated with the first string d
4. foldl1 takes the first two elements of the string list a, applies (c b) to
   them and again applies (c b) to the new string and the third element of the
   list and so on
5. The result is a string obtained by concatenating the elements of the string
   list a with the character b. Example: ["sun","moon","earth"] 'i' returns
   "sunimooniearth"
-}


f3 :: Int -> Integer
f3 a = (filter b [1..]) !! a where
  b c = all d [1..c-1] where d e = gcd c e == 1
{-
1. Takes an Int as input and returns the prime number in that index, where the
   list starts with 1 with index 0. Example: input 3 gives 5
2. function d with input e checks whether gcd of c and e is 1 or in other words
whether e is a relative prime with c
3. so, all d [1..c-1] checks whether any of the numbers between 1..c-1 divides c,
   if any number except 1 divides c, then the result is false otherwise true
4. so, function b checks whether the number is prime or not (though it includes 1)
5. (filter b [1..]) creates a list of primes and !! a returns the prime number at
   index a

   Péter: Note that filtering is delayed until it becomes absolutely necessary
   when the ath element is fetched, and even then filtering ends at this
   element.
-}

{-
 Péter: Brilliant recursions, but not very Haskellish, because they work with
 indices. See my versions below.
-}

reflexive :: [(Int,Int)] -> Bool
reflexive l = reflexiveAux l 0

reflexiveAux :: [(Int,Int)] -> Int -> Bool
reflexiveAux l n
  | n == length l                               = True
  | (any (== (fst(l !! n), fst(l !! n))) l) &&
    (any (== (snd (l !! n), snd (l !! n))) l)   = reflexiveAux l (n+1)
  | otherwise                                   = False


symmetric :: [(Int,Int)] -> Bool
symmetric l = symmetricAux l 0

symmetricAux :: [(Int,Int)] -> Int -> Bool
symmetricAux l n
  | n == length l                         = True
  | any (== (snd(l !! n), fst(l !! n))) l = symmetricAux l (n+1)
  | otherwise                             = False

transitive :: [(Int,Int)] -> Bool
transitive l = transitiveAux l 0 1

transitiveAux :: [(Int,Int)] -> Int -> Int -> Bool
transitiveAux l m n
  | n == length l                        = True
  | snd(l !! m) == fst(l !! n) &&
    any (== (fst(l !! m),snd(l !! n))) l = transitiveAux l (m+1) (n+1)
  | snd(l !! m) /= fst(l !! n) = transitiveAux l (m) (n+1)
  | otherwise                            = False

equivalence :: [(Int,Int)] -> Bool
equivalence l
  | reflexive l && symmetric l && transitive l = True
  | otherwise                                  = False
-- Péter: The guards are redundant here. You can go just like:

-- Péter: A version without indices:

reflexive' :: [(Int,Int)] -> Bool
reflexive' l = all (`elem` l) [(x,x) | x <-(dom l ++ rng l)] where
  dom :: [(Int,Int)] -> [Int]
  dom l = [fst x | x <- l]
  rng :: [(Int,Int)] -> [Int]
  rng l = [snd x | x <- l]

symmetric' :: [(Int,Int)] -> Bool
symmetric' l = all (`elem` l) [(n,m) | (m,n) <- l]

-- this one is much slower than yours
transitive' :: [(Int,Int)] -> Bool
transitive' l = all (`elem` l)
                [(n,m) | n <- (field l), m <- (field l), connected n m l] where
  field :: [(Int,Int)] -> [Int]
  field l = [fst x | x <- l] ++ [snd x | x <- l]
  connected :: Int -> Int -> [(Int,Int)] -> Bool
  connected n m l = any (connects n m l) (field l)
  connects :: Int -> Int -> [(Int,Int)] -> Int -> Bool
  connects n m l k = (n,k) `elem` l && (k,m) `elem` l

equivalence' :: [(Int,Int)] -> Bool
equivalence' l = reflexive l && symmetric l && transitive l
