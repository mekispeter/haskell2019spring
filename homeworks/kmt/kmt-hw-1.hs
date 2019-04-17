{-
Solutions for the excercises in haskell_hw_1.pdf, keeping mind the restrictions mentioned in
the 3rd bullet point. 
-}

age :: Int -> Int
age date = 2019 - date

{-
Int -> Int leaves no room for apologies regarding counterintuitive output:
λ> age 1986
33
λ> age 2019
0
λ> age 2100
-81
-}

sqsum :: Int -> (Int -> Int)
sqsum a b =
  sqr a + sqr b
  where sqr x = x * x

{-
λ> sqsum (-1) 1
2
λ> sqsum 5 7
74
-}

abs' :: Int -> Int
abs' x
  | x >= 0     = x
  | otherwise  = x * (-1)

{-
λ> abs' (-10)
10
λ> abs' 10
10
-}

not' :: Bool -> Bool
not' p
  | p          = False
  | otherwise  = True

import Test.QuickCheck
prop_not p = not p == not' p
{-
λ> quickCheck (prop_not)
+++ OK, passed 100 tests.
-}

{-

TODO: write a quickcheck test to check not == not'
λ> not' True
False
λ> not' False
True
λ> not' 1
    * No instance for (Num Bool) arising from the literal `1'
    * In the first argument of not', namely `1'
      In the expression: not' 1
      In an equation for `it': it = not' 1
-}

age' :: Int -> (Bool -> Int)
age' date hadBday
  | hadBday    = age date
  | otherwise  = age (date - 1)


{-
λ> age' 2011 True
8
λ> age' 1923 False
95
-}

matImpl :: Bool -> (Bool -> Bool)
matImpl p q
  | p && not' q  = False
  | otherwise    = True

{-
λ> matImpl True False
False
λ> matImpl True True
True
λ> matImpl False True
True
λ> matImpl False False
True
-}

matImpl' :: Bool -> (Bool -> Bool)
matImpl' p q
  | and' p (not' q)  = False
  | otherwise        = True
  where and' p q
          | not' p     = False
          | not' q     = False
          | otherwise  = True

{-
λ> matImpl' True False
False
λ> matImpl' True True
True
λ> matImpl' False True
True
λ> matImpl' False False
True
-}

sign :: Int -> String
sign n
  | n < 0      = "negative"
  | n > 0      = "positive"
  | otherwise  = "zero"

{-
λ> sign (-1)
"negative"
λ> sign 1
"positive"
λ> sign 0
"zero"
λ> sign (-0) 
"zero"
-- Okay, why is this?
λ> :info (-)
class Num a where
  ...
  (-) :: a -> a -> a
  ...
  	-- Defined in `GHC.Num'
infixl 6 -
-- google google:
http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Num.html
...
    (-) = minusNatural
...
-- and then:
http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Natural.html#minusNatural_
minusNatural x         (NatS# 0##) = x
minusNatural (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> NatS# l
    _           -> underflowError
minusNatural (NatS# _) (NatJ# _) = underflowError
minusNatural (NatJ# x) (NatS# y)
    = bigNatToNatural (minusBigNatWord x y)
minusNatural (NatJ# x) (NatJ# y)
    = bigNatToNatural (minusBigNat     x y)
{-# CONSTANT_FOLDED minusNatural #-}
-- well okay this is a bit too much as of now?
-}


grade :: Int -> String
grade gn -- for grade number
  | gn == 5 = "excellent"
  | gn == 4 = "good"
  | gn == 3 = "fair"
  | gn == 2 = "sufficient"
  | gn == 1 = "fail"
  | otherwise = "not a grade!"

{-
λ> grade 4
"good"
λ> grade 7
"not a grade!"
λ> 
-}


pythtriple :: Int -> Int -> Int -> Bool
pythtriple a b c =
  sqr a + sqr b == sqr c
  where
    sqr x = x * x

{-
λ> pythtriple 5 12 13
True
λ> pythtriple 6 12 13
False
-}

max3 :: Int -> Int -> Int -> Int
max3 a b c =
  max' a (max' b c)
  where
    max' x y
      | x > y      = x
      | otherwise  = y

{-
λ> max3 5 7 3
7
λ> max3 (-5) (-7) (-3)
-3
-}


-- Three functions that are not on this list.

-- I'd like to write a function that given integer a provides the next integer in the Collatz sequence
-- In order to avoid using any builtins, we'd need to define division without reminder. Unfortunately,
-- for that to work, I have to use one "advanced feature" of haskell, but I hope that that's covered in
-- the next session.

-- whole division, no error handling, cause we have no fancier types
-- let's say, it's not ready for production...
div' :: Int -> Int -> Int
div' a b = div'' a b 0
  where
    div'' a b c
      | a < b      = c
      | a == b     = 1 + c
      | otherwise  = div'' (a - b) b (1 + c)

{-
λ> div' 10 3
3
λ> div' 1 1
1
λ> div' 10 1
10
λ> div' 52 2
26
-}

-- if we have div already, let's define modulo, as used in programming
mod' :: Int -> Int -> Int
mod' a k = a - ( k * (div' a k ))
{-
λ> mod' 3 2
1
λ> mod' 6 2
0
λ> mod' 42 3
0
λ> mod' 42 7
0
λ> mod' 42 6
0
λ> mod' 42 9
6
-}

-- and now, in order to get the next integer in the Collatz sequence:
collatzNext :: Int -> Int
collatzNext n
  | mod' n 2 == 0  = div' n 2
  | otherwise      = 3 * n + 1
  
