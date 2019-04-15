-- Age calculator
age :: Int -> Int
age yr
  | yr > 0     = 2018 - yr
  | otherwise  = 2018 + yr

-- Absolute value calculator
abs' :: Int -> Int
abs' n
  | n > 0      = n
  | otherwise  = (n*(-1))

-- Conditional evaluator
cond :: Bool -> (Bool -> Bool)
cond first second
  | first == True && second == False    = False
  | otherwise                           = True

-- Adds "-ban" or "-ben" to words, according to the backness of their vowels
-- (doesn't include rules for vowels with an accent, e.g. "á", because I don't
-- know how to decode these)
baen :: String -> String
baen wrd
  | elem 'a' wrd || elem 'o' wrd || elem 'u' wrd  = wrd ++ "ban"
  | otherwise  = wrd ++ "ben"

-- Gives feedback for a guess-the-number game
num :: Int -> String
num n
  | n > 23    = "Too big!"
  | n < 23    = "Too small!"
  | otherwise = "You got it!"

-- Points to the argument whose value is in between the other two on the
-- number line.
middle :: Int -> (Int -> (Int -> String))
middle a b c
  | (b < a && a < c) || (c < a && a < b) = "The middle one is the first\
  \ argument."
  | (a < b && b < c) || (c < b && b < a) = "The middle one is the second\
  \ argument."
  | (a < c && c < b) || (b < c && c < a) = "The middle one is the third\
  \ argument."
  | otherwise = "There is no middle one."
