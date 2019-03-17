-- HW 1
fibonacci :: Integer -> Integer
fibonacci n = fibonacciAux 0 1 n

fibonacciAux :: Integer -> Integer -> Integer -> Integer
fibonacciAux n1 n2 i
  | i == 0 = n2
  | otherwise = fibonacciAux (n2) (n2+n1) (i-1)


-- HW 4
all' :: (Integer -> Bool) -> [Integer] -> Bool
all' f l
  | l == [] = True
  | f (head l) = all' f (tail l)
  | otherwise = False


-- HW 5
any' :: (Integer -> Bool) -> [Integer] -> Bool
any' f l
  | l == [] = False
  | f (head l) == False = any' f (tail l)
  | otherwise = True

-- HW 6
zip' :: [String] -> [Integer] -> [(String,Integer)]
zip' s i
  | s == [] && i == [] = []
  | otherwise = (head s, head i) : zip' (tail s) (tail i)


-- HW 7
zipWith' :: (Integer -> Integer -> Integer) -> [Integer] -> [Integer] -> [Integer]
zipWith' f s i
  | s == [] && i == [] = []
  | otherwise = (f (head s) (head i)) : zipWith' f (tail s) (tail i)


-- HW 9
takeWhile' :: (Integer -> Bool) -> [Integer] -> [Integer]
takeWhile' f l
  | l == [] = []
  | f (head l) = head l : takeWhile' f (tail l)
  | otherwise = takeWhile' f (tail l)


-- HW 10
evensLeq :: Integer -> [Integer]
evensLeq m = [n | n <- [1..m], even n, n < m]


-- HW 11 Took some of your example code
primesLeq :: Integer -> [Integer]
primesLeq m = [n | n <- [2..m], properDivisors n == []] where
              properDivisors :: Integer -> [Integer]
              properDivisors n = [j | j <- [2..(n-1)], mod n j == 0, n <= m]

-- HW 12
slice :: String -> [String]
slice s = [[t] | t <- s, True]


isVowel :: Char -> Bool
isVowel x
  | x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' = True
  | otherwise = False


-- HW 13
separate :: String -> (String, String)
separate s = ([x | x <- s, isVowel x], [y | y <- s, isVowel y == False && y /= ' '])


count :: Char -> String -> Int
count m l = length (filter (== m) l)


-- HW 14
reduceString :: String -> String
reduceString s
  | count (head s) s >=1 = head s : reduceString (filter (/= head s) s)
  | otherwise = [t | t <- s , count t s == 1 ]


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (n:s) = quicksort low ++ [n] ++ quicksort high where
                  low = filter (<n) s
                  high = filter (>=n) s
