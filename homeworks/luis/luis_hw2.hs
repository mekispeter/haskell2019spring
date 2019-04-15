{--this dyadic function takes a character c and a number n and returns
a string of n times c's --}

repetition :: Int -> Char -> [Char]
repetition n c
  | n == 0      = ""
  | otherwise   = c : repetition (n-1) c


-- this generates a palindrome
mirror :: String -> [Char]
mirror word
  |word == ""   = word
  |otherwise = mirror (tail word) ++ [head word]

-- this uses the previous function to check if 2 words are palindromic
palindromeCheck :: String -> String -> Bool
palindromeCheck sample1 sample2
    | sample1 == mirror sample2 = True
    | otherwise                 = False

{-- The above test worked even thought the output of mirror is a [Char]
therefore character lists and strings are treated as equivalent? --}


--fibonacci algorithms, different ones. Peter and I worked on this together

-- Fibonnacci using lists
fibonacci2 :: Integer -> (Integer, Integer)
fibonacci2 1 = (0,1)
fibonacci2 n = (fst old + snd old, fst old) where
  old=fibonacci2 (n-1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci n = fst (fibonacci2 n)

fibonacci_slow :: Integer -> Integer
fibonacci_slow 0 = 0
fibonacci_slow 1 = 1
fibonacci_slow n = fibonacci_slow (n-1) + fibonacci_slow (n-2)

fibonacci_list :: Integer -> [Integer]
fibonacci_list 1 = [1,0]
fibonacci_list 0 = [0]
fibonacci_list n = new : old where
  new = head old + head (tail old)
  old = fibonacci_list (n-1)

fibonacci' n = head (fibonacci_list n)
