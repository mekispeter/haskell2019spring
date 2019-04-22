{-
  Functional Programming for Logicians, 2019 Spring
  Basic IO code for Homework 8
-}

-- This is just a toy function to show how various types are handled in IO.
foo :: [String] -> Int -> Bool
foo xs n = maximum (map length xs) < n

main :: IO()
main = do
  par1 <- getLine
  par2 <- getLine
  -- Remember the '$' notation? It is used to spare brackets.
  putStrLn $ show $ foo (read par1 :: [String]) (read par2 :: Int)
