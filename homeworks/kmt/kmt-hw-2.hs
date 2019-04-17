{-
Solutions for the excercises in haskell_hw_2.pdf, keeping mind the restrictions mentioned in
the 4th bullet point. 
-}

-- this definition is incorrect, but it's hard to see anything correct with this type
-- there won't be a char that we can give back for the empty string.
{-
λ> :t ""
"" :: [Char]
λ> :t "a"
"a" :: [Char]
λ> :t head "a"
head "a" :: Char
λ> head ""
*** Exception: Prelude.head: empty list
-}
my_last :: String -> Char
my_last (s:"") = s
my_last (s:ss) = my_last ss

my_last' :: String -> Char
my_last' s
  | tail s == ""  = head s
  | otherwise     = my_last' $ tail s


{-
λ> my_last' "Haskell"
'l'
λ> my_last "@"
'@'
-}
  
  
my_length :: String -> Integer
my_length s
  | s == "" = 0
  | otherwise = 1 + (my_length $ tail s)

my_length' :: String -> Integer
my_length' "" = 0
my_length' (s:ss) = 1 + (my_length' ss)

{-
λ> my_length "0"
1
λ> my_length ""
0
λ> my_length "Gottlob Frege"
13
λ> my_length' "Gottlob Frege"
13
-}

-- mis-typed in pdf? Char -> String -> String
-- I mean, a number returned as string won't be very useful afterwards...
count_occur :: Char -> String -> Integer
count_occur c s
  | s == "" = 0
  | head s == c = 1 + (count_occur c $ tail s)
  | otherwise = count_occur c $ tail s

{-
λ> count_occur 'o' "Scooby-Doo"
4
λ> count_occur '0' "3.14159265358979323846"
0
-}

my_repeat :: Char -> Integer -> String
my_repeat c 0 = ""
my_repeat c n = [c] ++ my_repeat c (n-1)

{-
λ> my_repeat 'c' 3
"ccc"
λ> my_repeat 'c' 0
""
-}

nth_char :: String -> Integer -> Char
nth_char s 0 = head s
nth_char "" n = '!'
nth_char s n = nth_char (tail s) (n-1)

{-
-- call order in homework is reversed compared to the type signature
λ> nth_char "Hello World!" 0
'H'
λ> nth_char "Hello World!" 7
'o'
λ> nth_char "Hello World!" 100
'!'
-}

is_bin :: String -> Bool
is_bin "" = False
is_bin ('0':s) = False
is_bin (s:ss) = is_bin' ss
  where is_bin' "" = True
        is_bin' ('0':t) = is_bin' t
        is_bin' ('1':t) = is_bin' t


{-
λ> is_bin "00"
False
λ> is_bin "01"
False
λ> is_bin "10"
True
λ> is_bin ""
False
-}

reverse' :: String -> String
reverse' "" = ""
reverse' (x:xs) = reverse' xs ++ [x]

{-
λ> reverse' "Gottlob Frege"
"egerF bolttoG"
λ> reverse' "ahha"
"ahha"
λ> prop_rev s = reverse s == reverse' s
λ> quickCheck(prop_rev)
+++ OK, passed 100 tests.
-}

-- though haskell prefers camelCase and snake_case is frowned upon...
is_palindr :: String -> Bool
is_palindr s = s == reverse' s

{-
λ> is_palindr "kayak"
True
λ> is_palindr "(#_#)"
False
-}

-- this is pretty inefficient
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

{-
λ> fibonacci 10
55
λ> fibonacci 30
832040
-}

fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' n = fib 0 1 (n-1)
  where fib a b 0 = b
        fib a b n = fib b (a+b) (n-1)

{-
more efficient recursive version with an off by one error...
λ> fibonacci' 30
832040
-}


well_parenth :: String -> Bool
well_parenth s
  -- a well parenthesized string is a string with no mismatched parens
  | s == ""    = True
  | otherwise  = well_p_aux 0 s
  where
    well_p_aux counter s
      | counter < 0              = False
      | s == "" && counter == 0  = True
      | s == "" && counter /= 0  = False
      | head s == '('            = well_p_aux (counter+1) (tail s)
      | head s == ')'            = well_p_aux (counter-1) (tail s)
      | otherwise                = well_p_aux counter (tail s)

{-
λ> well_parenth "(()(()()))"
True
λ> well_parenth "(()(()(()))"
False
λ> well_parenth ""
True
λ> well_parenth "asdas asdad as"
True
-- i wonder if there's a non counter based solution
-}

begins :: String -> String -> Bool
begins full "" = True
begins (f:ull) (s:ub)
  | f == s = begins ull ub
  | otherwise = False

{-
-- the first useful quickcheck test, and I came up with the property myself! :)
λ> prop_begin f s = begins f s == (take (length s) f == s)
λ> quickCheck(prop_begin)
*** Failed! (after 4 tests and 3 shrinks):
Exception:
  <interactive>:(1329,1)-(1332,21): Non-exhaustive patterns in function begins
""
"a"
-}

begins' :: String -> String -> Bool
begins' "" _     = False
begins' full ""  = True
begins' (f:ull) (s:ub)
  | f == s = begins' ull ub
  | otherwise = False

{-
-- wow, this is more clever than I thought!
λ> prop_begin f s = begins' f s == (take (length s) f == s)
λ> quickCheck(prop_begin)
*** Failed! Falsifiable (after 1 test):
""
""
-}

begins'' :: String -> String -> Bool
begins'' "" ""    = True
begins'' "" _     = False
begins'' full ""  = True
begins'' (f:ull) (s:ub)
  | f == s = begins'' ull ub
  | otherwise = False

{-
λ> prop_begin f s = begins'' f s == (take (length s) f == s)
λ> quickCheck(prop_begin)
+++ OK, passed 100 tests.
-}

-- reusing solutions from the previous homework
-- still not good as this type is incorrect for divsion (div by zero is not handled)
-- not to mention the infinite loop with negative numbers...
div' :: Int -> Int -> Int
div' a b = div'' a b 0
  where
    div'' a b c
      | a < b      = c
      | a == b     = 1 + c
      | otherwise  = div'' (a - b) b (1 + c)

mod' :: Int -> Int -> Int
mod' a k = a - ( k * (div' a k ))

divides :: Int -> Int -> Bool
divides a b = mod' b a == 0

{-
λ> divides 3 2019
True
λ> divides 2 2019
False
oh, by the way, don't do a divides (-2) 2019, as my
div' is ... lacking in correctness
TODO: write a correct division?
ACHIEVEMENT UNLOCKED: a typechecked infinite loop in haskell :)
-}

-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Show.html
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Read.html

digitsum :: Integer -> Integer
digitsum i = ds $ show i
  where
    ds s
      | s == "" = 0
      | otherwise = (read [head s] :: Integer) + ds (tail s)

{-
λ> digitsum 2019
12
λ> digitsum 1999
28
λ> digitsum (-1999)
*** Exception: Prelude.read: no parse
-}

-- why Int at the end in the homework?
-- straight from wikipedia
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (mod' a b)

{-
λ> gcd 102 74
2
λ> gcd 1024 768
256
-}

-- excercise 15, example uses the name begins, which is ... counterintuitive
contains :: String -> String -> Bool
-- setting to False for qucikcheck, though it is intuitive this way too:
-- a string is contained in an other string if all the characters appear in the other
-- in sequence at some starting point...
--contains "" ""    = False
contains "" "" = True
contains "" s     = True
contains subs ""  = False
contains (su:bs) (s:ss)
  | su == s    = contains bs ss
  | otherwise  = contains (su:bs) ss

{-
λ> contains "Wit" "Ludwig Wittgenstein"
True
λ> contains "True" "Donald Trump"
False
-- okay, how to test against a known implementation?
-- after installing the strings library Data.Strings will be available
λ> import Test.QuickCheck
λ> import Data.Strings
λ> prop_cont a b = contains a b == (snd ( strBreak a b ) /= "")
λ> quickCheck(prop_cont)
+++ OK, passed 100 tests.
-}

bsucc :: String -> String
bsucc str = reverse' (bsucc' (reverse' str))
  where
    bsucc' s
      | s == "" = "1"
      | [head s] == "0" = ("1" ++ (tail s))
      | otherwise = "0" ++ (bsucc' (tail s))
{-

λ> bsucc "0"
"1"
λ> bsucc "1"
"10"
λ> bsucc "111"
"1000"
λ> bsucc "1101"
"1110"
λ> bsucc "1001"
"1010"
-- :)
λ> bsucc ""
"1"
Unfortunately, based on a cursory search, binary literals are provided only by
extensions: {-# LANGUAGE BinaryLiterals #-}
and it's not that straightforward to write a quickcheck test for now. Perhaps later
-}


-- For some reason, i cannot decrease the length without losing the correct pattern
bprev str = reverse' (bprev' (reverse' str))
  where
    bprev' s
      | s == "" = "0"
      | [head s] == "1" = ("0" ++ (tail s))
      | otherwise = "1" ++ (bprev' (tail s))

bplusP ba bb
  | contains "1" bb = bplusP (bsucc ba) (bprev bb)
  | otherwise =  ba

-- in the kitchen i got the idea: use the XOR, XOR the inputs and XOR that result
-- together with the carry. now that i think about it, i think i saw this structure
-- in circuit diagrams...
bxor :: String -> String -> String
bxor a b
  | a == b = "0"
  | otherwise = "1"


bplus :: String -> String -> String
bplus a b = reverse' (bplus' (reverse' a) (reverse' b) "0")
  where
    bplus' "" "" carry = carry
    bplus' (a:as) "" carry = (bxor [a] carry) ++ as
    bplus' "" (b:bs) carry = (bxor [b] carry) ++ bs
    bplus' (a:as) (b:bs) carry = ab ++ bplus' as bs carry'
      where
        ab = (bxor [a] (bxor [b] carry))
        carry' = if ((([a] == "1" || [b] == "1") && carry == "1") || ([a] == "1" && [b] == "1" && carry == "0")) then "1" else "0"

{-
λ> bplus "100" "101"
"1001"
λ> bplus "1" "1"
"10"
-- though that hand tabulated condition in carry' is horrible
-- if i'd have time, i'd simplify it, or at least feed it to z3 or some
-- other engine to have it expressed in terms of bxor & simplify...
-}



-- TODO: figure out a way, how to generate binary number strings with quickcheck so i can test
-- the equality of bplus and bplusP

is_prime :: Integer -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n = is_prime' n 2  -- n limit test
  where
    is_prime' n test
      | test > ( round $ sqrt $ fromInteger n)  = True
      | n `mod` test == 0                       = False
      | otherwise                               = is_prime' n (test+1)

-- finding the place where to put the type conversion function took a better half of a day.
-- the error messages are indeed counterintuitive at first

next_prime :: Integer -> Integer
next_prime n
  | is_prime (n+1)  = n+1
  | otherwise       = next_prime (n+1)

{-
λ> next_prime 2
3
λ> next_prime 2019
2027
-}


nprimes :: Integer -> [Integer]
nprimes n = nprimes' n [2]
  where
    nprimes' n lst
      | n == 1 = lst
      | otherwise = nprimes' (n-1) (lst ++ [next_prime $ last lst])
{-
λ> nprimes 2
[2,3]
λ> nprimes 12
[2,3,5,7,11,13,17,19,23,29,31,37]
-}
