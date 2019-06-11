-- Wellformedness-check for standard propositional formulas.
-- Alphabet: p + D & V = ~ ( )
-- Auxiliary letters: T F

-- Example:
-- Input:  ((pD(p+Dp++))D((pDp+)D(pDp++)))
-- Output: T
-- Input:  (p~Dp+)
-- Output: F

p+    ->  p
(pxp) ->  p     | x: D & V =
~p    ->  p
x     ->  F     | x: + D & V = ~ ( )
p     ->  T
TT    ->  F
TF    ->  F
FT    ->  F
FF    ->  F
0     ->* 0
