ld :: Int -> Int
ld n = auxLd n 2
  where
    auxLd :: Int -> Int -> Int
    auxLd n k
      | rem n k == 0 = k
      | otherwise    = auxLd n (k+1)

isprime :: Int -> Bool
isprime 1 = False
isprime n = ld n == n

nextprime :: Int -> Int
nextprime n
  | isprime (n + 1) = n + 1
  | otherwise       = nextprime (n + 1)

nprimes :: Int -> [Int]
nprimes n = auxNprimes n 1 []
  where
    auxNprimes :: Int -> Int -> [Int] -> [Int]
    auxNprimes n m lst
      | n == 0    = lst
      | otherwise = auxNprimes (n-1) (nextprime m) (lst ++[nextprime m])

reverse' :: String -> String
reverse' str = reverseAux str []
  where
    reverseAux :: String -> String -> String
    reverseAux str lst
      | str == [] = lst
      | otherwise = reverseAux (tail str) (head str : lst)

-- also defining with the syntactic way:

reverse'' :: String -> String
reverse'' (x:xs) = reverseAux' (x:xs) []
  where
    reverseAux' :: String -> String -> String
    reverseAux' [] ys     = ys
    reverseAux' (x:xs) ys = reverseAux' xs (x:ys)


digitAux :: Int -> String
digitAux num = show num

digAux :: String -> Int
digAux str
  |  str == [] = 0
  | otherwise = num + digAux (tail str)
  where num = (read [head str]) :: Int

digitsum :: Int -> Int
digitsum num = digAux (digitAux num)

-- some auxiliary function for defining binary pred, succ, add, mult

suffix :: [Char] -> Int -> [Char]
suffix str n
  | n == 0    = str
  | otherwise = suffix (str ++ ['0']) (n-1)

bin :: [Char] -> [Char]
bin num = auxBin num 0
  where
    auxBin :: [Char] -> Int -> [Char]
    auxBin num n
      | num == []       = "1" ++ suffix num n
      | last num == '0' = (init num) ++ "1"
      | otherwise       = auxBin (init num) (n + 1)

bsucc :: String -> String
bsucc n
  | length n > length (bin n) = suffix (bin n) (length n - length (bin n))
  | otherwise                 = bin n

bin' :: [Char] -> [Char]
bin' num
  | length num == 1 = "0"
  | last num == '1' = (init num) ++ "0"
  | otherwise       = bin' (init num)


suffix' :: [Char] -> Int -> [Char]
suffix' str n
  | n == 0    = str
  | otherwise = suffix' (str ++ ['1']) (n-1)

bpredAux :: String -> String
bpredAux n = suffix' (bin' n) (length n - length (bin' n))

bpred :: String -> String
bpred lst
  | bpredAux lst == "0"        = bpredAux lst
  | head (bpredAux lst) == '0' = tail (bpredAux lst)
  | otherwise                  = bpredAux lst

---

bplus :: String -> String -> String
bplus n m
  | m == "0"  = n
  | otherwise = bplus (bsucc n) (bpred m)

btimes :: String -> String -> String
btimes n m
  | m == "0"  = "0"
  | otherwise = bplus n (btimes n (bpred m))
