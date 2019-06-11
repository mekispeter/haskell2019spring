-- Parenthesis checker, another version. This one preserves the original word.
-- Alphabet: a b c ( )
-- Auxiliary letters: M i
-- M is a cursor that runs from the left to the right, and counts parenthesis
-- depth. I is an index that indicates depth.

-- Examples:
-- Input:   (a(a(ba)a(ab))a)
-- Output:  (a(a(ba)a(ab))a)T
-- Input:   (a(a(ba)a(ab))a))
-- Output:  (a(a(ba)a(ab))a))F

Ix  ->  xI    | x: abcdefghijklmnopqrtstuvwxyz ( )
Mx  ->  xM    | x: abcdefghijklmnopqrtstuvwxyz
M(  ->  (IM
IM) ->  )M
M)  ->  )F
IM  ->  F
Fx  ->  xF    | x: abcdefghijklmnopqrtstuvwxyz ( )
M   ->  T
F   ->* F
T   ->* T
0   ->  M
