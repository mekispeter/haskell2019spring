{-
Functional Programming for Logicians, 2019 February 11
These are the functions defined in the first session. You can call them from
the ghci prompt, after loading this file, e.g.
> :l feb11.hs
[long echo]
> greeting_p "Haskell"
"Hello, Haskell!"
> large 99
False
>
-}

-- This is not a function, since it has no arguments.
greeting :: String
greeting = "Hello world!"

-- This is a function that takes a string, and returns a message that includes
-- the string.
greeting_p :: String -> String
greeting_p name = "Hello, " ++ name ++ "!"

-- This function is a unary predicate; it takes a number, and tells whether
-- it is small. The small-large limit is set to 100.
large :: Int -> Bool
large n
  | n < 100     = False
  | otherwise   = True

-- In this version, the limit is given as a second argument. In fact, this is
-- the same as the "larger than" relation for the Int type.
large_pc :: Int -> (Int -> Bool)
large_pc n limit
  | n < limit     = False
  | otherwise     = True
