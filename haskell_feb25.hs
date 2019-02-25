{-
  Functional Programming for Logicians, 2019 February 11
  These are the functions defined in the third session.
  We switched from underscore naming (eg. 'some_function')
  to camelcase naming (eg. 'someFunction') to see if it is better.
-}

-- The first k naturals divisible by n:
-- a considerably slow version
firstDiv :: Integer -> Integer -> [Integer]
firstDiv k n = firstDivAux k n 0
-- We need an extra parameter; we introduce it via an auxiliary function.
-- This is a standard trick.
firstDivAux :: Integer -> Integer -> Integer -> [Integer]
firstDivAux k n i
  | k == 0          = []
  | mod i n == 0    = i : firstDivAux (k-1) n (i+1)
  | otherwise       = firstDivAux k n (i+1)

-- This is a much faster version; instead of k * n recursive calls,
-- it only requires k calls.
firstDiv' :: Integer -> Integer -> [Integer]
firstDiv' k n = firstDivAux' k n 0
firstDivAux' :: Integer -> Integer -> Integer -> [Integer]
firstDivAux' k n i
  | k == 0          = []
  | otherwise       = i : firstDivAux' (k-1) n (i+n)

-- Parenthesis checking, first version. We proceed through the string
--  from left to right, keeping track of the number of parentheses opened.
parenth :: String -> Bool
parenth s = parenthAux s 0
-- Once again, we need an extra variable.
parenthAux :: String -> Integer -> Bool
parenthAux "" 0        = True
parenthAux "" i        = False
parenthAux ('(':s) i   = parenthAux s (i+1)
parenthAux (')':s) 0   = False
parenthAux (')':s) i   = parenthAux s (i-1)
parenthAux (c:s) (-1)  = False
parenthAux (c:s) i     = parenthAux s i

-- Now we take another approach to the parenthesis problem. The new
-- algorithm will find and erase simple patterns in the string in a
-- recursive manner. We don't proceed from left to right.

-- We start with a function that checks whether a string sBig begins with
-- another string sSmall.
beginsWith :: String -> String -> Bool
beginsWith sBig sSmall
  | sSmall == ""              = True
  | sBig == ""                = False
  | head sSmall == head sBig  = beginsWith (tail sBig) (tail sSmall)
  | otherwise                 = False

-- Based on the previous one, it will be easy to define a function that
-- checks whether sSmall is a substring of sBig.
subString :: String -> String -> Bool
subString sBig sSmall
  | beginsWith sBig sSmall  = True
  | sBig == ""              = False
  | otherwise               = subString (tail sBig) sSmall
