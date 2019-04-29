flip' :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
flip' f n m = f m n

all' :: (String -> Bool) -> [String] -> Bool
all' f lst
  | lst == []          = True
  | not (f (head lst)) = False
  | otherwise          = all' f (tail lst)

any' :: (Integer -> Bool) -> [Integer] -> Bool
any' f []      = True
any' f (x:xs)
  | not (f x ) = False
  | otherwise  = any' f xs

zipWith' ::  (Integer -> Integer -> Integer) -> [Integer] -> [Integer] -> [Integer]
zipWith' f lst []        = []
zipWith' f [] lst        = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

foldl' :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldl' f n []     = n
foldl' f n (x:xs) = foldl' f (f n x) xs

takeWhile' :: (Integer -> Bool) -> [Integer] -> [Integer]
takeWhile' f [] = []
takeWhile' f (x:xs)
  | f x         = x : takeWhile' f xs
  | otherwise   = []

evensLeq :: Integer -> [Integer]
evensLeq n = [i | i <- [1 ..n], even i]

-- previously defined
ld :: Integer -> Integer
ld n = auxLd n 2
  where
    auxLd :: Integer -> Integer -> Integer
    auxLd n k
      | rem n k == 0 = k
      | otherwise    = auxLd n (k+1)

-- previously defined
isprime :: Integer -> Bool
isprime 1 = False
isprime n = ld n == n

primesLeq :: Integer -> [Integer]
primesLeq n = [i | i <- [1 ..n], isprime i]

slice :: String -> [String]
slice str = [[char] | char <- str ]

separate :: String -> (String, String)
separate str = ([i | i <- str, elem i vowel], [i | i <- str, not (elem i (vowel ++ [' ']))]) where
  vowel = ['a','e','i','o','u']

-- previously defined
end :: Int -> [a] -> [a]
end 0 lst    = []
end n []     = []
end n lst    =  end (n-1) (init lst) ++ [last lst]


-- fibonacci algorithm without bifurcation
fib :: Integer -> Integer
fib n = fibAux n [1,1] 1
  where
    fibAux :: Integer -> [Integer] -> Integer -> Integer
    fibAux 0 lst c = 0
    fibAux 1 lst c = 1
    fibAux n lst c
      | n - 2 == c = sum (end 2 lst)
      | otherwise  = fibAux n (lst ++ [sum (end 2 lst)]) (c + 1)

-- even faster algorithm
fib' :: Integer -> Integer
fib' n = fibAux n [1,1] 1
  where
    fibAux :: Integer -> [Integer] -> Integer -> Integer
    fibAux 0 lst c = 0
    fibAux 1 lst c = 1
    fibAux 2 lst c = 1
    fibAux n lst c
      | n - 2  == c   = sum (take 2 lst)
      | otherwise  = fibAux n (sum (take 2 lst) : lst) (c + 1)

--[1,1,2,3,5,8,13 ]

fib'' :: Integer -> Integer
fib'' n = fibAux n [1,1] 1
  where
    fibAux :: Integer -> [Integer] -> Integer -> Integer
    fibAux 0 lst c = 0
    fibAux 1 lst c = 1
    fibAux 2 lst c = 1
    fibAux n lst c
      | n - 2  == c   = sum (take 2 lst)
      | otherwise  = fibAux n (sum (take 2 lst) : [head lst]) (c + 1)

sumLengths :: [[a]] -> Int
sumLengths lst = sum [length i | i <- lst]

primes :: [Integer]
primes = [i | i <- [1..], isprime i]
