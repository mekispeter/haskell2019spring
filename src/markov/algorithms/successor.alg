-- Returns the successor of a decimal numeral.
-- 0 is taken for the empty word; hence the o.
-- Alphabet: o 1 2 3 4 5 6 7 8 9
-- Auxiliary letters: @ #

-- Input:  2o19
-- Output: 2o2o

o# ->* 1
1# ->* 2
2# ->* 3
3# ->* 4
4# ->* 5
5# ->* 6
6# ->* 7
7# ->* 8
8# ->* 9
9# -> #o
#  ->* 1
@x -> x@ | x: o 1 2 3 4 5 6 7 8 9
@ -> #
0 -> @
