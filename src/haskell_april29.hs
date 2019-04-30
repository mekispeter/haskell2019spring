{-
  Functional Programming for Logicians 2019 Spring
  Code created in the April 29 session

  We solved a homework, and then did a bit of IO.
-}

{-
  Homework 8, exercise 8: instantiating the Applicative class for the Tree
  type. We made two versions both of which satisfy the Applicative laws. The
  two versions differ only in whether we use the treeAddZ function suggested
  by Zalan or the treeAddP function suggested by Peter.

  First we need to repeat the definitions from earlier sessions.
-}
data Tree a = Leaf a | Node a (Tree a) (Tree a)
              deriving (Show, Read, Eq)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x t1 t2) = Node (f x) (fmap f t1) (fmap f t2)

instance Applicative Tree where
  pure x = Leaf x
  Leaf f <*> tv = fmap f tv
  Node f left right <*> tv
        = treeAddZ (fmap f tv) (left <*> tv) (right <*> tv)

-- This is Zalan's version:
treeAddZ :: Tree a -> Tree a -> Tree a -> Tree a
treeAddZ (Leaf x) t2 t3 = Node x t2 t3
treeAddZ (Node x left right) t2 t3
        = Node x (treeAddZ left t2 t3) right

-- Peter's version:
treeAddP :: Tree a -> Tree a -> Tree a -> Tree a
treeAddP (Leaf x) t2 t3 = Node x t2 t3
treeAddP (Node x left right) t2 t3
        = Node x (treeAddP left t2 t3)
                 (treeAddP right t2 t3)

-- Two sample trees to make function calls from ghci easier:
funcTree :: Tree (Integer -> Integer)
funcTree = Node (+1) (Leaf (+ (-1))) (Leaf (*2))
valTree :: Tree Integer
valTree  = Node 1 (Leaf 2) (Leaf 3)
-- Now try: > funcTree <*> valTree

-- this is here to explain what the id function does in the Applicative laws:
id' :: a -> a
id' x = x

{-
  IO examples
  The standard way to handle IO is to use 'do' blocks and imperative code. This
  is just syntactic sugar for monadic actions. We will see in the next session
  how to desugar the code.

  In the session, we named the functions below main, main2, and main3. This was
  more misleading than helpful.
-}

-- Greeting the user:
greetingIO :: IO ()
greetingIO = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)

-- Returning the factorial of a value given by the user:
factIO :: IO()
factIO = do
  putStrLn "Factorial of what?"
  num <- getLine
  let fact = factorial $ read num :: Integer
  putStrLn $ show fact
  where
    -- This is the old factorial function from February.
    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial n = n * factorial (n-1)

{-
  Finally a little guessing game. We didn't finish this one. The present code
  is simpler than the one we made in the session.
-}

-- This is the main function that runs the game.
gameIO :: IO()
gameIO = do
  putStrLn "Guessing game: find an integer between 1 and 100."
  loop 72 -- This should be a random number, but we don't know yet how to
          -- generate random numbers.
  where
    -- This is the game loop. Works with simple recursion.
    loop:: Integer -> IO()
    loop n = do
      putStr("Guess: ")
      mi <- getLine
      let m = read mi :: Integer
      if m == n
        then putStrLn("Yepp, that was it!")
        else do
          putStrLn (measure m n)
          loop n
    -- This function just makes the interaction more fluent.
    measure :: Integer -> Integer -> String
    measure m n
      | n < m     = "Less!"
      | otherwise = "More!"
