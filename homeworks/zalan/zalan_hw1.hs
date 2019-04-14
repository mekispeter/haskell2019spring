max3 :: Int -> Int -> Int -> Int
max3 x y z = max x (max y z)

pythtriple :: Int -> Int -> Int -> Bool
pythtriple x y z = (x^2) + (y^2) == (z^2)

sign :: Int -> String
sign n
  |n > 0 = "positive"
  |n < 0 = "negative"

age :: Int -> Bool -> Int
age n m
  | m == True = 2019 - n
  | otherwise = (2019 - n) - 1

imp :: Bool -> Bool -> Bool
imp p q
  | p == True && q == False = False
  | otherwise               = True

maxlist :: [Int] -> Int
maxlist [x] = x
maxlist lst = max (head lst) (maxlist (tail lst))
