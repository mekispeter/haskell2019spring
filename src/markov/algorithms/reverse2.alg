-- This algorithm reverses a word, keeping the original.

-- Alphabet: abcdefghijklmnopqrstuvwxyz
-- Auxiliary letters: I @ #

-- Example:
-- Input:   bacca
-- Output:  baccaIaccab

x#y ->  yx#     | x: abcdefghijklmnopqrstuvwxyz | y: abcdefghijklmnopqrstuvwxyz I
#   ->  0
x@  ->  @xx#     | x: abcdefghijklmnopqrstuvwxyz
@   ->* 0
Ix  ->  xI      | x: abcdefghijklmnopqrstuvwxyz
I   ->  @I
0   ->  I
