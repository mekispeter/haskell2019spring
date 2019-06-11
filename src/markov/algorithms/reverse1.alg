-- This algorithm reverses a word.

-- Alphabet: abcdefghijklmnopqrstuvwxyz
-- Auxiliary letters: I @ #

-- Example:
-- Input:   bacca
-- Output:  accab

x#y ->  yx#     | x: abcdefghijklmnopqrstuvwxyz | y: abcdefghijklmnopqrstuvwxyz
#   ->  0
x@  ->  @x#     | x: abcdefghijklmnopqrstuvwxyz
@   ->* 0
Ix  ->  xI      | x: abcdefghijklmnopqrstuvwxyz
I   ->  @
0   ->  I
