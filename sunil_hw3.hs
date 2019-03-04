last_char :: String -> Char
last_char s
  | tail s == "" = head s
  | otherwise    = last_char (tail s)


string_length :: String -> Integer
string_length s
  | s == ""   = 0
  | otherwise = 1 + string_length (tail s)


-- HW 1
drop' :: String -> Integer -> String
drop' s n = dropAux s n 0

dropAux :: String -> Integer -> Integer -> String
dropAux s n i
  | i == n    = s
  | otherwise = dropAux (tail s) n (i+1)


-- HW 2
take' :: String -> Integer -> String
take' s n = takeAux s n 0


takeAux :: String -> Integer -> Integer -> String
takeAux s n i
  | s == ""             = ""
  | i == n              = ""
  | otherwise           = head s : takeAux (tail s) n (i+1)


-- HW 3
div' :: Integer -> Integer -> Integer
div' m n = divAux m n 0


divAux :: Integer -> Integer -> Integer -> Integer
divAux m n q
  | m < n     = 0
  | m == n    = 1
  | otherwise = 1 + divAux (m-n) n (q+1)


-- HW 4
middleChar :: String -> Char
middleChar s
  | string_length s `rem` 2 == 0 = '!'
  | otherwise = last_char (take' s (((string_length s) `div` 2) + 1))


-- HW 5
nextTo :: String -> Char -> Char
nextTo s c
  | tail s == ""     = '!'
  | head s == c = head (tail s)
  | otherwise   = nextTo (tail s) c


-- HW 6
slice :: String -> [String]
slice s
  | s == ""   = []
  | otherwise = [head s] : slice (tail s)


-- HW 8
separate :: String -> (String, String)
separate s
  | s == ""                                           = ([],[])
  | head s == 'a' || head s == 'e' ||
      head s == 'i' || head s == 'o' || head s == 'u' = let x = head s in (x : fst (separate (tail s)), snd (separate (tail s)))
  | otherwise                                         = let x = head s in (fst (separate (tail s)), x: snd (separate (tail s)))


part :: String -> Char -> Bool
part s c
  | s == ""     = False
  | head s == c = True
  | otherwise   = part (tail s) c


-- HW 9
reducestring :: String -> String
reducestring s
  | tail s == ""           = [head s]
  | part (tail s) (head s) = reducestring (tail s)
  | otherwise              = head s : reducestring (tail s)


--HW 10
subStrings :: String -> [String]
subStrings s = subStringsAux s 0

subStringsAux :: String -> Int -> [String]
subStringsAux s i
  | s == ""       = []
  | i <= length s = take i s : subStringsAux s (i+1)
  | otherwise     = subStringsAux (tail s) 1
