-- This algorithm returns the sum of two binary numbers.
-- 0 is taken for the empty word; hence the o.
-- A surprisingly simple algorithm: only 23 rules (16 with variables).
-- Alphabet: o 1 +
-- Auxiliary letters: > @ & i # $

-- Input:  1oo1oo1+11o11o1
-- Output: 1o11o11o

-- Place indices travel left.
xi  ->  ix | x: o 1 +
-- $ travels left until it finds its place. Then it applies the successor
-- operation on what it finds.
x$i ->  $x | x: o 1
o$  ->  1
1$  ->  $o
$   ->  1
-- # travels left until it finds the +. Then it turns into $.
x#  ->  #x | x: o 1
+#  ->  $+
-- & travels left and counts places, until it finds a 1.
-- Then it turns into a #.
1&  ->  #o
o&  ->  &io
-- The sum is ready. < is introduced, and erases redundant symbols.
<x  ->  < | x: o i
<@  ->* 0
+&  ->  <
-- @ sends &s to the left.
@   ->  &@
-- Start: > travels to the right end, and turns into @.
>x  ->  x> | x: o 1 +
>   ->  @
0   ->  >
