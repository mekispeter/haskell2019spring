import Natural
import Data.List (transpose)

{-
  Syntax and Semantics of Standard Propositional Logic
  Functional Programming for Logicians 2019 Spring

  - syntax : definition of a formula as a tree with atomic formulas as
    leaves and connectives as nodes
  - semantics: evaluation of formulas in terms of evaluation of atoms --
    truth value, truth condition, truth table
  - logical concepts: tautology, equivalence, consequence
  - formula parser (TO DO)
-}

type Index = Natural

-- Formulas are either atomic, or negations, conjunctions, disjunctions,
-- conditionals, or biconditionals of formulas.
data Formula = At Index | Neg Formula | Conj Formula Formula |
            Disj Formula Formula | Cond Formula Formula | Bic Formula Formula
            deriving Eq

instance Show Formula where
  show (At n)       = 'p' : show n
  show (Neg f)      = "~ " ++ show f
  show (f `Conj` g) = '(' : show f ++ " & "   ++ show g ++ ")"
  show (f `Disj` g) = '(' : show f ++ " v "   ++ show g ++ ")"
  show (f `Cond` g) = '(' : show f ++ " -> "  ++ show g ++ ")"
  show (f `Bic` g)  = '(' : show f ++ " <-> " ++ show g ++ ")"
-- eg.
-- > sampleF1
-- (~ (p1 v p2) <-> (~ p1 & ~ p2))

-- Indices of atomic subformulas in a single formula
atoms :: Formula -> [Index]
atoms f = case f of
  At n        -> [n]
  Neg g       -> atoms g
  g `Conj` h  -> atoms g `redPlus` atoms h
  g `Disj` h  -> atoms g `redPlus` atoms h
  g `Cond` h  -> atoms g `redPlus` atoms h
  g `Bic`  h  -> atoms g `redPlus` atoms h
  -- eg.
  -- > atoms sampleF1
  -- [1,2]

-- Indices of atomic subformulas in a list of formulas
atomsL :: [Formula] -> [Index]
atomsL fs = redConcat $ map atoms fs

-- Reducing plus: only adds new elements
redPlus :: (Eq a) => [a] -> [a] -> [a]
redPlus xs ys = xs ++ filter (\y -> not $ y `elem` xs) ys
-- > redPlus [1,2,3] [4,3,5]
-- [1,2,3,4,5]

redConcat :: (Eq a) => [[a]] -> [a]
redConcat = foldr redPlus []
-- > redConcat [[1,2],[2,3,4],[1,3,5]]
-- [1,2,3,4,5]

type Eval = [(Index, Bool)]

-- Truth possibilities: list of all possible evaluations of a list of atoms
truthPoss :: [Index]-> [Eval]
truthPoss [i]     = [[(i, True)], [(i,False)]]
truthPoss (i:is)  = map ((i,True):) (truthPoss is) ++
                    map ((i,False):) (truthPoss is)
-- (We could import Data.Map.Strict, but that's kinda overkill for this.
-- Prelude has lookup function for lists of pairs.)
-- > truthPoss [1,2]
-- [[(1,True),(2,True)],[(1,True),(2,False)],
-- [(1,False),(2,True)],[(1,False),(2,False)]]

-- Truthvalue under an evaluation
truthValue :: Formula -> Eval -> Bool
truthValue (At i) eval        = case lookup i eval of
  Nothing                  -> error "Indices don't match!"
  Just bool                -> bool
truthValue (Neg f) eval       = not (truthValue f eval)
truthValue (f `Conj` g) eval  =     (truthValue f eval) && (truthValue g eval)
truthValue (f `Disj` g) eval  =     (truthValue f eval) || (truthValue g eval)
truthValue (f `Cond` g) eval  = not (truthValue f eval) || (truthValue g eval)
truthValue (f `Bic`  g) eval  =     (truthValue f eval) == (truthValue g eval)
-- > truthValue sampleF1 [(1,True),(2,False)]
-- True

-- Truth conditions: truth value under each possible evaluation
truthConditions :: Formula -> [Eval] -> [Bool]
truthConditions f evals = map (truthValue f) evals
-- > truthConditions sampleF1 (truthPoss (atoms sampleF1))
-- [True,True,True,True]

-- Tautology: truth under every evaluation
tautology :: Formula -> Bool
tautology f = minimum $ truthConditions f (truthPoss (atoms f))
-- > tautology sampleF1
-- True

equivalent :: Formula -> Formula -> Bool
equivalent f g = truthConditions f evals == truthConditions g evals where
  evals = truthPoss (atomsL [f,g])

truthTable :: [Formula] -> [[Bool]]
truthTable fs = transpose $ map (`truthConditions` (truthPoss $ atomsL fs)) fs
-- > truthTable [badChain1,badChain2,Neg badChain3]
-- [[True,True,False],[True,False,True],[False,True,False],[False,False,False],
-- [True,True,False],[True,True,True],[True,True,False],[True,True,False]]

consequence :: [Formula] -> Formula -> Bool
consequence fs g = not $ any (all (== True)) (truthTable $ Neg g : fs)
-- > consequence [goodChain1, goodChain2] goodChain3
-- True
-- > consequence [badChain1, badChain2] badChain3
-- False

sampleF1  = Neg ((At (toN 1)) `Disj` (At (toN 2)))
            `Bic`
            (Neg (At (toN 1)) `Conj` Neg (At (toN 2)))

sampleF2  = ((At (toN 1)) `Cond` (At (toN 2)))
            `Cond`
            (Neg (At (toN 2)) `Cond` Neg (At (toN 1)))

sampleF3  = ((At (toN 1)) `Cond` (At (toN 2)))
            `Cond`
            (Neg (At (toN 1)) `Cond` Neg (At (toN 2)))

goodChain1  =  (At (toN 1)) `Cond` (At (toN 2))
goodChain2  =  (At (toN 2)) `Cond` (At (toN 3))
goodChain3  =  (At (toN 1)) `Cond` (At (toN 3))

badChain1  =  (At (toN 1)) `Cond` (At (toN 2))
badChain2  =  (At (toN 1)) `Cond` (At (toN 3))
badChain3  =  (At (toN 2)) `Cond` (At (toN 3))


test :: Bool
test =  tautology sampleF1 && not (tautology sampleF3) &&
        equivalent sampleF1 sampleF2 && not (equivalent sampleF2 sampleF3) &&
        consequence [goodChain1, goodChain2] goodChain3 &&
        not (consequence [badChain1, badChain2] badChain3)
