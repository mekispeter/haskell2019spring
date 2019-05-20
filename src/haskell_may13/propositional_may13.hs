{-
  Functional Programming for Logicians, 2019 Spring
  May 13
  Project 1: An Implementation of propositional logic

-}

-- We made this module in the first 30 minutes of the session.
import Natural

-- We use an alias for the Natural type, to avoid confusion in case we want to
-- use it for something else than indexing.
type Index = Natural

-- Formulas are trees, with atomic formulas at the leaves and connectives at
-- the nodes. We could use the Tree type from before; instead, we introduce a
-- brand new type.
data Formula =  At Index            | Neg Formula         |
                Con Formula Formula | Imp Formula Formula |
                Bic Formula Formula | Dis Formula Formula
                deriving Eq
{-
  Luis' idea was to introduce a type for atomic formulas. This would've resulted in the following variation:
  Data AtForm  = Atom Index deriving Eq
  data Formula =  At AtForm | Neg Formula |
                  Con Formula Formula | Imp Formula Formula |
                  Bic Formula Formula | Dis Formula Formula
                  deriving Eq
  This way everything becomes a little more complex. This pays off if you want
  to use atomic formulas separately from complex ones in your implementation.
-}

-- Instead of the derived show method, which is pretty unreadable, we write our
-- own, to make formulas as similar to their usual form as possib
instance Show Formula where
  show (At n)     = 'p' : show n
  show (Neg f)    = "~ " ++ show f
  show (Con f g)  = '(' : show f ++ " ^ " ++ show g ++ ")"
  show (Imp f g)  = '(' : show f ++ " -> " ++ show g ++ ")"
  show (Bic f g)  = '(' : show f ++ " <-> " ++ show g ++ ")"
  show (Dis f g)  = '(' : show f ++ " v " ++ show g ++ ")"

-- This is a test formula to check if everything goes well:
sample1 = Neg (At 1 `Con` At 2) `Bic` (Neg (At 1) `Dis` Neg (At 2))
-- > sample1
-- (~ (p1 ^ p2) <-> (~ p1 v ~ p2))

-- This function gets the indices of the atomic subformulas of a formula.
atoms :: Formula -> [Index]
atoms (At n) = [n]
atoms (Neg f) = atoms f
atoms (f `Con` g) = atoms f `reducePlus` atoms g
atoms (f `Imp` g) = atoms f `reducePlus` atoms g
atoms (f `Bic` g) = atoms f `reducePlus` atoms g
atoms (f `Dis` g) = atoms f `reducePlus` atoms g

-- This is a special version of list addition, onitting duplicates. The lambda
-- definition is ugly, any idea on how to improve will be welcome.
-- It may be a good idea to import Data.Set and use the union function (works
-- exactly the same way, preferring the first occurrence of each element);
-- especially for a large amount of indices.
reducePlus :: Eq a => [a] -> [a] -> [a]
reducePlus xs ys = xs ++ filter (\y -> not $ y `elem` xs) ys
-- > reducePlus [1,2,3] [4,3,5]
-- [1,2,3,4,5]

-- We want to represent the assignment of truth values in terms of ordered
-- pairs. A more advanced solution would be to import the Data.Map.Strict
-- module; a good idea especiallly if there are a lot of indices involved.
truthPoss :: [Index] -> [[(Index, Bool)]]
truthPoss []      = []
truthPoss [i]     = [[(i, True)], [(i, False)]]
truthPoss (i:is)  = undefined
-- We didn't have time to complete this definition. What do you think the
-- tricky last clause will be? The desired output is:
-- > truthPoss [1,2]
-- [[(1,True),(2,True)],[(1,True),(2,False)],
-- [(1,False),(2,True)],[(1,False),(2,False)]]
