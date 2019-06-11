module Natural where

data Natural = Zero | Succ Natural deriving (Eq, Ord)

instance Enum Natural where
  fromEnum Zero     = 0
  fromEnum (Succ n) = succ $ fromEnum n
  toEnum 0          = Zero
  toEnum n          = Succ $ toEnum (n-1)

instance Show Natural where
  show n            = show $ fromEnum n

readN :: String -> Natural
readN s = toEnum (read s :: Int)

toN :: Int -> Natural
toN = toEnum

instance Num Natural where
  n + Zero      = n
  n + Succ m    = Succ (n + m)
  n - Zero      = n
  n - Succ m
    | m < n     = pred (n - m)
  n * Zero      = Zero
  n * Succ m    = n * m + n
  abs n         = n
  signum Zero   = Zero
  signum n      = Succ Zero
  fromInteger n
    | 0 <= n    = toN (fromInteger n :: Int)
    | otherwise = error "negative to Natural"
