-- Wellformedness-check for propositional formulas in the Polish notation.
-- Alphabet: p + C K A E N
-- Auxiliary letters: T F

-- Example:
-- Input:  EKNpNp+NApp+
-- Output: T
-- Input:  CppN+
-- Output: F

p+    ->  p
xpp   ->  p     | x: C K A E
Np    ->  p
x     ->  F     | x: + C K A E N
p     ->  T
TT    ->  F
TF    ->  F
FT    ->  F
FF    ->  F
0     ->* 0
