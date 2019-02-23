-- HW 1 Last character
last_char :: String -> Char
last_char s
  | tail s == "" = head s
  | otherwise    = last_char (tail s)


-- HW 2 Length of a string
string_length :: String -> Integer
string_length s
  | s == ""   = 0
  | otherwise = 1 + string_length (tail s)


-- HW 3 Counts Occurence of Character
occurs :: Char -> String -> Integer
occurs a s
  | s == ""     = 0
  | head s /= a = occurs a (tail s)
  | otherwise   = 1 + occurs a (tail s)


-- HW 4 Repeats Character
repeat_char :: Char -> Integer -> String
repeat_char a n
  | n == 0    = ""
  | otherwise = a : repeat_char a (n-1)


-- HW 5 Returns nth character
nth_char :: Integer -> String -> Char
nth_char n s
  | n + 1 > string_length s = '!'
  | n == 0                  = head s
  | otherwise               = nth_char (n-1) (tail s)


-- HW 6 Check if valid binary number
check_binary :: String -> Bool
check_binary s
  | s == "0" || s == "1"           = True
  | head s /= '0' && head s /= '1' = False
  | otherwise                      = check_binary (tail s)


-- HW 7 Reverse String
rvrs :: String -> String
rvrs r
  | r == ""   = r
  | otherwise = rvrs (tail r) ++ [head r]


-- HW 8 Check for palindrome
is_palindrome :: String -> Bool
is_palindrome p
  | p == rvrs p = True
  | otherwise   = False


-- HW 9 nth element of fibonacci sequence
fibonacci :: Integer -> Integer
fibonacci n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fibonacci (n-1) + fibonacci (n-2)


-- HW 10 Correctly parenthesised (Doesn't work)
-- well_parenth :: String -> Bool
-- well_parenth s
--   | s == "()" || s == "" = True
--   | s == "(" || s == ")" = False
--   | head s == '(' && head (tail s) == ')' = well_parenth(tail (tail s))
--   | head (rvrs s) == '(' && head (tail (rvrs s)) == ')' = well_parenth(rvrs(tail (tail (rvrs s))))
--   | head s == '(' && head (rvrs s) == ')' = well_parenth(rvrs(tail (rvrs (tail s))))
--   | otherwise = False


-- HW 11 Checks whether second string begins with first
begins :: String -> String -> Bool
begins r s
  | r == ""          = True
  | head r == head s = (tail r) `begins` (tail s)
  | otherwise        = False


-- HW 12 check if a number divides another
divides :: Integer -> Integer -> Bool
divides n m
  | n == 0    = False
  | n == 1    = True
  | n == m    = True
  | n > m     = False
  | otherwise = n `divides` (m-n)


-- HW 13 sums digits of integer
digit_sum :: Integer -> Integer
digit_sum n
  | n == 0    = 0
  | otherwise = (n `rem` 10) + digit_sum (n `div` 10)


{- HW 14 Greatest common divisor (Note: I don't understand why I get
 stack over flow error when the gcd is 1) -}
g_c_d :: Integer -> Integer -> Integer
g_c_d n m
  | n <= 1 || m <= 1 = 1
  | n `divides` m    = n
  | m `divides` n    = m
  | n < m            = n `g_c_d` m-n
  | otherwise        = m `g_c_d` n-m


g_c_d_mod :: Integer -> Integer -> Integer
g_c_d_mod n m
  | m == 0    = n
  | otherwise = m `g_c_d_mod` (n `mod` m)


-- HW 15 Checks whether first string is part of the second
part :: String -> String -> Bool
part r s
  | (r == "" && s == "") || r == ""                = True
  | s == ""                                        = False
  | head r /= head s                               = r `part` (tail s)
  | head r == head s && (tail r) `begins` (tail s) = True
  | otherwise                                      = False

-- HW 16 Successor of binary
bsucc :: String -> String
bsucc s
  | s == "1"           = "10"
  | last_char s == '0' = rvrs ('1' : tail (rvrs s))
  | otherwise          = bsucc(rvrs(tail (rvrs s))) ++ "0"


-- HW 17 Sum of two binary numbers
bsum :: String -> String -> String
bsum n m
  | n == "1" && m == "1"                     = "10"
  | (m == "" && n /= "")                     = n
  | (n == "" && m /= "")                     = m
  | last_char n == '0' && last_char m == '0' = (rvrs (tail (rvrs n))) `bsum` (rvrs(tail (rvrs m))) ++ "0"
  | last_char n == '1' && last_char m == '0' = (rvrs (tail (rvrs n))) `bsum` (rvrs(tail (rvrs m))) ++ "1"
  | last_char n == '0' && last_char m == '1' = (rvrs (tail (rvrs n))) `bsum` (rvrs(tail (rvrs m))) ++ "1"
  | (last_char n == '1' && last_char m == '1')
      && (string_length n > string_length m) = (bsucc(rvrs(tail(rvrs n)))) `bsum` (rvrs(tail(rvrs m))) ++ "0"
  | otherwise                                = (rvrs(tail(rvrs n))) `bsum` (bsucc(rvrs(tail(rvrs m)))) ++ "0"


-- HW 18 Check for prime (sorry didn't use recursion)
prime :: Integer -> Bool
prime n
  | n <= 1    = False
  | n == 2    = True
  | otherwise = map (`divides` n) [2..((n `div` 2) + 1)] == map (<2) [2..((n `div` 2) + 1)]


-- HW 19 Next Prime
next_prime :: Integer -> Integer
next_prime n
  | prime (n+1) = n+1
  | otherwise   = next_prime (n+1)


-- HW 20 Returns list of first n primes
n_primes :: Integer -> [Integer]
n_primes n
  | n == 1    = [2]
  | otherwise = next_prime (head (n_primes (n-1))) : (n_primes (n-1))


-- Own HW 1 Twin Primes
twin_prime :: Integer -> Integer -> Bool
twin_prime n m
  | g_c_d_mod n m == 1 = True
  | otherwise          = False


-- Own HW 2 Number of times a number divides another
times_divide :: Integer -> Integer -> Integer
times_divide n m
  | n `divides` m = 1 + (times_divide n (m `div` n))
  | otherwise = 0


-- Own HW 3 Power
power :: Integer -> Integer -> Integer
power n p
  | p == 1 = n
  | otherwise = n * power n (p-1)
