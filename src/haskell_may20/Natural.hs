{-
  Functional Programming for Logicians, 2019 Spring
  May 13 Session
-}

module Natural where

-- Instead of using Int or Integer for indices, which allow for negative
-- numbers, we define our own version of natural numbers. The type definition
-- is straightforward, but we need to instantiate quite a lot of classes.

data Natural = Zero | Succ Natural
  deriving (Eq, Ord)

instance Enum Natural where
  succ              = Succ
  pred Zero         = error
    "Zero has no predecessor!"
  pred (Succ n)     = n
  fromEnum Zero     = 0
  fromEnum (Succ n) = fromEnum n + 1
  toEnum n
    | n < 0         = error
      "No negative naturals!"
    | n == 0        = Zero
    | otherwise     = Succ $ toEnum (n-1)

instance Num Natural where
  n + Zero      = n
  n + Succ m    = Succ (n + m)
  n * Zero      = Zero
  n * Succ m    = n * m + n
  n - Zero      = n
  n - Succ m
    | n <= m    = error "No negative naturals!"
    | otherwise = pred (n - m)
  abs n         = n
  signum Zero   = Zero
  signum n      = Succ Zero
  fromInteger n
    | n < 0         = error
      "No negative naturals!"
    | n == 0        = Zero
    | otherwise     = Succ $ fromInteger (n-1)

instance Show Natural where
  show n = show $ fromEnum n
