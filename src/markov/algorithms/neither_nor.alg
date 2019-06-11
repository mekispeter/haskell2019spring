-- This algorithm converts formulas of standard propositional
-- logic into equivalent formulas where the only connective is
-- joint denial (also called NOR or neither-nor).

-- Alphabet: p + ~ D & V = ( )
-- Auxiliary letters: I i j m n o * ^ d

-- The conversion has two steps:
-- 1  Standard binary connectives are replaced with joint denial
--    and negation of the antecedent, the consequent, or the whole
--    subformula:
--    from  |  to             | auxiliary letters
--    ------+-----------------+------------------
--    (X&Y) | (~XI~Y)         | m n j
--    (XDY) | ~(~XIY)         | m j
--    (XVY) | ~(XIY)          | n j
--    (X=Y) | ((~XIY)I(XI~Y)) | o *
-- 2  Negations are replaced with joint denial:
--    ~X    | (XIX)           | ^ d i
-- Meanwhile, double negations keep being erased.

-- Examples:
-- Input:   (p&p+)
-- Halfway: (~pI~p+)
-- Output:  ((pIp)I(p+Ip+))
-- Input:   ~~(p&(p+D~p++))
-- Halfway: (~pI(~p+Ip++)
-- Output:  ((pIp)I((p+Ip+)I(p++Ip++))
-- Input:   (p=~p+)
-- Halfway: ((~pI~p+)I(pIp+))
-- Output:  (((pIp)I(p+Ip+))I(pIp+))

ix -> xi | x: p + ~ I ( ) D & V = e
xj -> jx | x: p + ~ I ( ) D & V =

xy -> yx | x: p + ~ I D & V = | y: m n

)y -> yj) | y: m n
(yjj -> yj( | y: m n
(mj -> ~(
(nj -> (~

xo -> ox | x: p + ~ = I

)o -> oj)
(oj -> o(
(o -> ((~^I(id

xey -> eyx | x: p + ~ ( ) | y: p + ~ I ( )
^Iex -> x^I | x: p + ~ I ( )
Iex -> exI | x: p + ~ I ( )

dx -> exxd | x: p + ~

idI -> eIIid
id*I -> eII~id

d( -> e((id
iid) -> e))id
id) -> e)))

d -> )
^ -> 0
~~ -> 0

D -> mjnjI
& -> njI~
V -> mjI
= -> o*I
~ -> (^Id

0 ->* 0
