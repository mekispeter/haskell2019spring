{-
Added by Peter:
This is pretty cool. I see you're already using recursion. Don't worry, 
there'll be harder exercises for the next week. Your version of max3 is 
brilliant, and makes maxn kinda trivial. Leap years are a little more complicated 
than your function assumes: cf. https://en.wikipedia.org/wiki/Leap_year#Algorithm
-}

square :: Int -> Int
square m = m*m

max2 :: Int -> Int -> Int
max2 n1 n2
  | n1 > n2 = n1
  | n2 > n1 = n2
  | otherwise = n1

-- HW 8 Grade
grade :: Int -> String
grade g
  | g == 5 = "Excellent"
  | g == 4 = "Good"
  | g == 3 = "Fair"
  | g == 2 = "Sufficient"
  | g == 1 = "Fail"
  | otherwise = "not a grade!"

-- HW 9 Pythagorean Triple
pythagorean_triple :: Int -> Int -> Int -> Bool
pythagorean_triple n1 n2 n3
  | square n1 + square n2 == square n3 = True
  | otherwise = False

-- HW 10 Maximum of 3 integers
max3 :: Int -> Int -> Int -> Int
max3 n1 n2 n3
  | n3 > max2 n1 n2 = n3
  | otherwise = max2 n1 n2

-- Own HW 1 Factorial
factorial :: Int -> Int
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n-1)

-- Own HW 2 Decimal to Binary
binary :: Int -> String
binary n
  | n == 0 = "0"
  | n == 1 = "1"
  | n `rem` 2 == 0 = binary (n `div` 2) ++ "0"
  | otherwise = binary (n `div` 2) ++ "1"

  -- Own HW 3 Check if Leap year
  -- Example: 2016, 2012, 2000, 2400 are leap years and 2100 and 2200 are not
leap_year1 :: Int -> Bool
leap_year1 y
  | (y `rem` 4 /= 0)   = False
  | (y `rem` 100 == 0) && (y `rem` 400 /= 0) = False
  | otherwise = True
