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
