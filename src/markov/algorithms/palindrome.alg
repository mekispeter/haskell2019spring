-- Palindrome checker.
-- Alphabet: a b c
-- Auxiliary letter: I
-- Examples:
-- in:  abacaba
-- out: T
-- in:  bababa
-- out: F

-- I carries the first letter towards the end of the word.
Ixy -> yIx | y: a b c | x: a b c
-- I finds the letter it carries at the end of the word.
xIx -> 0   | x: a b c
-- I finds another letter at the end of the word.
-- The word is not a palindrome.
aIx -> F   | x: b c
bIx -> F   | x: a c
cIx -> F   | x: a b
-- F deletes any other letter.
xF -> F    | x: a b c
-- Bad case: the algorithm ends with a single F.
-- The word was not a palindrome.
F ->* F
-- Good case 1: there remains a single middle letter.
-- The word was a palindrome.
Ix ->* T   | x: a b c
-- Good case 2: A single I remains.
-- There were an even number of pairs, all matching their pairs.
-- The word was a palindrome.
I ->* T
-- Start: I is introduced.
0 -> I
