-- This algorithm translates a formula from the Polish notation to the
-- standard one. It assumes that the input formula is well-formed.

-- Alphabet: p + C K A E N
-- Auxiliary letters: m D & V = ~ c k a e i v w d l b f

-- p, p+, p++, ...: propositional parameters
-- Polish connectives: C conditional, K conjunction, A disjunction,
-- E biconditional, N negation
-- Standard connectives: D conditional, & conjunction, V disjunction,
-- = biconditional, ~ negation
-- First carriage letters for connectives: c conditional, k conjunction,
-- a disjunction, e biconditional
-- Second carriage letters for connectives: d conditional, l conjunction,
-- b disjunction, f biconditional
-- m: main cursor
-- i: depth meter
-- v: letter to find the places of closing brackets
-- w: second version of v

-- Examples:
-- +---------------------+---------------------------------+
-- |        input        |             output              |
-- +---------------------+---------------------------------+
-- |        Cpp+         |             (pDp+)              |
-- +---------------------+---------------------------------+
-- |       NKNpNp+       |            ~(~p&~p+)            |
-- +---------------------+---------------------------------+
-- |     NANp+Cp++p      |         ~(~p+V(p++Dp))          |
-- +---------------------+---------------------------------+
-- | CCpCp+p++CCpp+Cpp++ | ((pD(p+Dp++))D((pDp+)D(pDp++))) |
-- +---------------------+---------------------------------+

-- Depth indices jump through propositional parameters and Polish connectives.
ix  ->  xi    | x: p + C K A E N
-- First carriage letters travel through binary Polish connectives;
-- depth is incremented.
yx  ->  xiy   | x: C K A E | y: c k a e v
-- First carriage letters travel through a Polish negation; depth doesn't change.
xN  ->  Nx    | x: c k a e v
-- First carriage letters reach a propositional parameter; they turn
-- into their second versions.
cp -> pd
kp -> pl
ap -> pb
ep -> pf
vp -> pw
-- Second carriage letters jump through indices.
x+  ->  +x    | x: d l b f w
-- Second carriage connectives with depth index 2 or more are turned back into
-- the corresponding first carriage connectives with depth index one.
iid ->  ic
iil ->  ik
iib ->  ia
iif ->  ie
-- Second carriage connectives with depth index 1 turn into the corresponding
-- standard notation connectives, and start a v that will find the place of
-- the closing bracket.
id  ->  Dv
il  ->  &v
ib  ->  Vv
if  ->  =v
-- v jumps through Polish binary connectives with its depth index
-- incremented.
vC  ->  Civ
vK  ->  Kiv
vA  ->  Aiv
vE  ->  Eiv
--  -v jumps through Polish negation without incrementing depth index.
vN-> Nv
-- If w has at least depth 1, turns back into v.
iw  ->  v
-- Otherwise it turns into a closing bracket.
w   ->  )
-- m travels through propositional letters along with their indices,
-- binary connectives of the standard notation, and closing brackets
-- without changing them.
mx  ->  xm    | x: p+D&V=)
-- m reaches a binary Polish connective. It leaves an opening bracket behind,
-- and starts a first carriage version of itself, along with
-- an i to measure depth.
mC  ->  (mic
mK  ->  (mik
mA  ->  (mia
mE  ->  (mie
-- m reaches a Polish negation symbol, and turns it into a standard one.
-- There are no punctuation issues.
mN  ->  ~m
-- m reaches the end of the formula: the algorithm ends.
m   ->* 0
-- The algorithm starts with the introduction of a cursor m at the head
-- of the formula.
0   ->  m
