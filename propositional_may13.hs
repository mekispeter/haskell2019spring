import Natural

type Index = Natural
data Formula =  At Index |
                Neg Formula |
                Con Formula Formula |
                Imp Formula Formula |
                Bic Formula Formula |
                Dis Formula Formula
                deriving (Eq)

instance Show Formula where
  show (At n)     = 'p' : show n
  show (Neg f)    = "~ " ++ show f
  show (Con f g)  = '(' : show f ++ " ^ " ++ show g ++ ")"
  show (Imp f g)  = '(' : show f ++ " -> " ++ show g ++ ")"
  show (Bic f g)  = '(' : show f ++ " <-> " ++ show g ++ ")"
  show (Dis f g)  = '(' : show f ++ " v " ++ show g ++ ")"

sample1 = Neg (At 1 `Con` At 2) `Bic` (Neg (At 1) `Dis` Neg (At 2))

atoms :: Formula -> [Index]
