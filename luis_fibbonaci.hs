-- Fibonnacci using lists
fibonacci2 :: Integer -> (Integer, Integer)
fibonacci2 1 = (1,1)
fibonacci2 n = (fst old + snd old, fst old) where
  old=fibonacci2 (n-1)

fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci n = snd (fibonacci2 n)

fibonacci_slow :: Integer -> Integer
fibonacci_slow 0 = 1
fibonacci_slow 1 = 1
fibonacci_slow n = fibonacci_slow (n-1) + fibonacci_slow (n-2)
