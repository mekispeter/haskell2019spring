{-
Functional Programming for Logicians, 2019 February 11
These are the functions defined in the second session.-}

-- factorial: the hello world of recursion
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = factorial (n-1) * n

-- Stroke arithmetic

-- zero is the empty string
szero :: String
szero = ""

-- the successor operation adds one stroke
ssucc :: String -> String
ssucc n = '|' : n

-- one is a singe stroke
sone :: String
sone = ssucc szero

-- the predecessor function removes one stroke
spred :: String -> String
spred n
  | n == sone = szero
  | otherwise = tail n

-- addition is defined recursively in terms of the successor operation
splus :: String -> String -> String
splus n m
  | m == szero = n
  | otherwise  = ssucc (splus n (spred m))

-- multiplication is defined recursively in terms of addition
stimes :: String -> String -> String
stimes n m
  | m == szero = szero
  | otherwise  = (n `stimes` (spred m)) `splus` n

-- the longer string of strokes represents the larger number
-- relations are functions to Booleans
sless :: String -> String -> Bool
sless n m
  | m == szero = False
  | n == szero = True
  | otherwise  = (spred n) `sless` (spred m)

-- subtraction is a partial operation; we assume that m is less than or equals n
sminus :: String -> String -> String
sminus n m
  | m == szero  = n
  | otherwise   = (spred n) `sminus` (spred m)

-- finally, we use subtraction to define the divisibility relation
sdivides :: String -> String -> Bool
sdivides n m
  | n `sless` m = False
  | n == m      = True
  | otherwise   = (n `sminus` m) `sdivides` m
