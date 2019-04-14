-- Example
occurs :: Char -> String -> Bool
occurs c ""     = False
occurs c (d:s)  = if c == d then True else occurs c s

-- Returns last character of string
my_last :: String -> Char
my_last ""     = ' '
my_last (c:s)  = if s == "" then c else my_last s

-- Returns number of characters in string
my_length :: String -> Integer
my_length ""  = 0
my_length s   = (my_length (tail s)) + 1

-- Counts occurrences of character in string
count_occur :: Char -> String -> Integer
count_occur c ""     = 0
count_occur c (d:s)  = if c == d then (count_occur c s) + 1 else count_occur c s

-- Returns string composed of a given number of characters
rpeat :: Char -> Integer -> String
rpeat c 0  = ""
rpeat c n  = c : rpeat c (n-1)

-- Returns nth character of a string
nth_char :: String -> Integer -> Char
nth_char s 0      = head s
nth_char (c:s) n  =  if n+1 <= my_length (c:s) then nth_char s (n-1) else '!'

-- Checks if a string is a binary number
is_bin :: String -> Bool
is_bin "1"    = True
is_bin "0"    = True
is_bin (c:s)  = if c == '1' && is_bin ((head s):"") == True then True else False
is_bin ""     = False

-- Reverses string
rverse :: String -> String
rverse ""     = ""
rverse (c:s)  = rverse s ++ c:""

-- Checks if string is a palindrome
is_palindr :: String -> Bool
is_palindr s  = if rverse s == s then True else False

-- (own idea) Turns a string of 'c'-s into a string of "Clap!"-s
clap :: String -> String
clap ""     = ""
clap (c:s)  = if c == 'c' then clap s ++ "Clap!" else clap s

-- (own idea) Character rules for encrypter below
alph_1 :: Char -> Char
alph_1 ' '  = ' '
alph_1 '?'  = '?'
alph_1 '.'  = '.'
alph_1 'a'  = 'b'
alph_1 'b'  = 'c'
alph_1 'c'  = 'd'
alph_1 'd'  = 'e'
alph_1 'e'  = 'f'
alph_1 'g'  = 'h'
alph_1 'h'  = 'i'
alph_1 'i'  = 'j'
alph_1 'j'  = 'k'
alph_1 'k'  = 'l'
alph_1 'l'  = 'm'
alph_1 'm'  = 'n'
alph_1 'n'  = 'o'
alph_1 'o'  = 'p'
alph_1 'p'  = 'q'
alph_1 'q'  = 'r'
alph_1 'r'  = 's'
alph_1 's'  = 't'
alph_1 't'  = 'u'
alph_1 'u'  = 'v'
alph_1 'v'  = 'w'
alph_1 'w'  = 'x'
alph_1 'x'  = 'y'
alph_1 'y'  = 'z'
alph_1 'z'  = 'a'

-- (own idea) Dummy encrypter
encrypt :: String -> String
encrypt ""     = ""
encrypt (c:s)  = alph_1 c : encrypt s

-- (own idea) Converts string into Hungarian "madárnyelv", e.g. "tudsz igy beszelni?" to "tuvudsz ivigy beveszevelnivi?", "cogito ergo sum" to "covogivitovo evergovo suvum"
madar :: String -> String
madar ""     = ""
madar (c:s)  = if occurs c "aeiou" == True then c:('v':(c:(madar s))) else c : madar s

-- (own idea) Removes all 'e'-s from a string, based on George Perec's lipogrammatic novel "A Void"
avoid :: String -> String
avoid ""     = ""
avoid (c:s)  = if c == 'e' || c == 'E' then avoid s else c : avoid s
