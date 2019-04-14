{-
  Very good job! Almost everything works (nextto fails with a specific input),
  and your own functions are original and entertaining. Two recurring issues:
  - You tend to overuse 'if-then-else'. Embedded uses are especially ugly.
  - You tend to overuse the 'length' function.
  - Some of your code lines are very long. (80 characters is a standard limit.)
-}

-- Sample
pascal :: Int -> [[Int]]
pascal 1 = [[1]]
pascal n = prev ++ [pascal_next (last prev)] where
    prev = pascal (n-1)
    pascal_next xs = head xs : pascal_nf xs where
    pascal_next :: [Int] -> [Int]
    pascal_nf :: [Int] -> [Int]
    pascal_nf xs
      | xs == []       = []
      | tail xs == []  = [head xs]
      | otherwise      = (head xs + head (tail xs)) : pascal_nf (tail xs)

{-
  Péter: Note that the order of parameters in your version of take and drop
  is the opposit of Haskell's built-in versions.
-}
-- Drops first k characters of string
drop_hw :: String -> Int -> String
drop_hw s 0  = s
drop_hw s n  = if n <= length s then tail (drop_hw s (n-1)) else ""

-- Takes first k characters of a string
take_hw :: String -> Int -> String
take_hw s 0  = ""
take_hw s n  = if n <= length s then head s : take_hw (tail s) (n-1) else s

{-
  Péter: Your version of div is clear and stratghtforward, but a bit
  complicated. Here's a simpler one:
  div_hw' :: Int -> Int -> Int
  div_hw' n m
    | n < m = 0
    | otherwise = 1 + div (n-m) m
-}
-- Does integer division on n and m
div_hw :: Int -> Int -> Int
div_hw n m  = div_aux n m 0
-- Auxiliary function for div_hw
div_aux :: Int -> Int -> Int -> Int
div_aux n 1 i  = n
div_aux n m i  = if i*m <= n && (i+1)*m > n then i else div_aux n m (i+1)

{-
  Péter: This works, but it is ugly! :) A recursive version:
  middlechar' :: String -> Char
  middlechar' ""  = '!'
  middlechar' c:"" = c
  middlechar' s   = middlechar' (init (tail s))
-}

middlechar :: String -> Char
middlechar ""     = '!'
-- Returns middle character of string, or exclamation mark if there isn't one
middlechar (c:s)  = if even (length (c:s)) then '!' else head (drop_hw (c:s) (div_hw (length (c:s)) 2))

{-
  Péter: There are three issues with this definition.
  1. It raises error for calls like
    > nextto "Trump" 'p'
  where first occurrence of the character is the last item in the string.
  2. Embedded if-then-else constructs are not easy to read. You can use guards
  to make the structure of the definition clearer.
  3. 'elem' is another recursive function, called at each recursive step.
  This makes your algorithm less effective than a more straightforward version
  which only checks the head of the string, like:
  nextto' :: String -> Char -> Char
  nextto' "" c        = '!'
  nextto' (d:"") c    = '!'
  nextto' (d:(e:s)) c
    | d == c          = e
    | otherwise       = nextto' (e:s) c
-}
-- Finds in a string the character that follows the first occurrence of a given character
nextto :: String -> Char -> Char
nextto "" c     = '!'
nextto (c:s) d  = if not (elem d (c:s)) then '!' else if c == d then head s else nextto s d

{-
  Péter: A version without 'length':
  punjoin' :: String -> String -> String
  punjoin' s z
    | beginsWith s z  = z
    | otherwise       = head s : punjoin' (tail s) z
    where
      beginsWith :: String -> String -> Bool
      beginsWith "" z = True
      beginsWith s "" = False
      beginsWith (c:s) (d:z)
        | c == d      = beginsWith s z
        | otherwise   = False
-}
{-- "Punjoins" two strings, i.e. if the last k letters of the first string are
the same as the first k letters of the second (for some k), then it
concatenates the first length-k letters of the first string, the identical
letters, and the last length-k letters of the second string, e.g. "ladder" and
"derrida" becomes "ladderrida", "Gottlob" and "lobster" becomes "Gottlobster". --}
punjoin :: String -> String -> String
punjoin s1 s2  = if s1 == s2 then s1 else punjoinAux s1 s2 1
-- Auxiliary for punjoin
punjoinAux :: String -> String -> Int -> String
punjoinAux s1 s2 k  = if k <= min (length s1) (length s2) then if drop_hw s1 ((length s1) - k) == take_hw s2 k then s1 ++ (drop_hw s2 k) else punjoinAux s1 s2 (k+1) else s1 ++ s2

{-
  Péter: A version without length:
  switch' :: String -> String -> String
  switch' s1 s2 = s2First ++ s1Rest ++ " " ++ s1First ++ s2Rest where
    (s1First,s1Rest) = splitAtVowel s1
    (s2First,s2Rest) = splitAtVowel s2
    splitAtVowel :: String -> (String,String)
    splitAtVowel "" = ("","")
    splitAtVowel (c:s)
      | c `elem` "aeiouAEIOU" = (c:"",s)
      | otherwise = (c:(fst recurs), snd recurs)
      where
        recurs = splitAtVowel s
-}
{-- In two strings switches the first parts that end in a vowel, e.g.
"pattogatott" "kukorica" becomes "kuttogatott pakorica", "logika es"
"tudomanyelmelet" becomes "tugika es lodomanyelmelet". If you apply it to your
last and first name you can find out the name of your evil twin. --}
switch :: String -> String -> String
switch "" ""  = ""
switch s1 s2  = snipAtVowel s2 ++ drop_hw s1 (length (snipAtVowel s1)) ++ (' ' : (snipAtVowel s1 ++ drop_hw s2 (length (snipAtVowel s2))))
-- Takes a string's first substring that ends in a vowel
snipAtVowel :: String -> String
snipAtVowel ""  = ""
snipAtVowel s   = if elem (head s) "aeiouAEIOU" then (head s) : "" else if elem (head (tail s)) "aeiouAEIOU" then (head s) : (head (tail s) : "") else head s : snipAtVowel (tail s)

{-
  Péter: You can use putStrLn to create a multiline output in ghci:
  > putStrLn (wordSquare "Haskell")

  Haskell
  askellH
  skellHa
  kellHas
  ellHask
  llHaske
  lHaskel
-}
{-- Creates a square based on the letters of a string so that the first row and
the first column spell out the word — but the newlines are automatically escaped
in GHCi, so it won't look like a square.  --}
wordSquare :: String -> String
wordSquare ""     = ""
wordSquare (c:s)  = wordSquareAux (c:s) (length (c:s))
-- Auxiliary for wordSquare
wordSquareAux :: String -> Int -> String
wordSquareAux (c:s) 0  = ""
wordSquareAux (c:s) n  = wordSquareAux (c:s) (n-1) ++ ('\n' : (drop_hw (c:s) (n-1) ++ take_hw (c:s) (n-1)))
