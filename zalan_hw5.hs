{-
  Péter: Very good job! Similar tasks will be much easier with parsing strings
  into trees though.
-}

{-
# In modal logic modal depth (MD) of a formula is defined as:
MD(p) = 0 (∀p p is atomic formula)
MD(¬φ) = MD(φ)
MD(φ & φ') = max(MD(φ), MD(φ'))

...

MD(Bφ) = 1 + MD(φ) where B abreviates 'box'
MD(Dφ) = 1 + MD(φ) where D abreviates 'diamond'

# wff
for atomic formulas use letters p,q,r...
negation of φ: -φ
conjunction of φ and φ': (φ&φ')
disjunction of φ and φ': (φvφ')
implication : (φ>φ')
modal φ: Bφ or Dφ

# Example
Main aim is to define a function called modalDepth :: String -> Int s.t. unfolding the definition of MD,
for a given wff input, say "-DD-(DB--B-p>(-q&DB-BDr))", modalDepth returns 6.

# Important: tha input formula should be a wff, otherwise it doesn't work. In the updated version
I will include solutions for the ill formed ones.

-}

type Property = String -> Bool

-- connectives (negation not included here)
conn = ['>','&','v']

-- complex modal formula
modForm :: Property
modForm (x:xs) = x =='B' || x =='D'

-- simple modal formulas: built up by using only negation and box
simpleMod :: Property
simpleMod str = if [i | i <- str, elem i conn] == [] && length str > 1 then True else False

-- modal formula
modal :: Property
modal str = simpleMod str || modForm str

-- simple formula:
simpleForm :: Property
simpleForm str = if [i | i <- str, elem i conn] == [] && not (simpleMod str) then True else False


sORm :: Property
sORm str = simpleForm str || modal str

mainC :: String -> (String, Int)
mainC str = if sORm str then (str, 0) else mainAux str 0 0 where
  mainAux :: String -> Int -> Int -> (String, Int)
  mainAux str n c
    | n == 1 && elem (head str) conn = ([head str], c)
    | head str == '('                = mainAux (tail str) (n+1) (c+1)
    | head str == ')'                = mainAux (tail str) (n-1) (c+1)
    | otherwise                      = mainAux (tail str) n (c+1)


-- give back the "second" half of a string depending on an input number
sec :: Int -> String -> String
sec n str = secAux 0 str
  where
    secAux :: Int -> String -> String
    secAux c str
      | n == c    = tail str
      | str == [] = error "string is too short"
      | otherwise = secAux (c + 1) (tail str)

split :: String  -> (String, String)
split str = if simpleForm str then undefined else  (first, second)
 where
  first   = tail(take (snd (mainC str)) str)
  second  = init(sec (snd (mainC str)) str)

negForm :: Property
negForm (x:xs) = x == '-'

-- modal depth
modalDepth :: String -> Int
modalDepth str = modalAux str 0 where
  modalAux :: String -> Int -> Int
  modalAux str n
    | simpleForm str        = n + 0
    | negForm str           = modalAux (tail str) n
    | mainConnective == "&" = max first second
    | mainConnective == "v" = max first second
    | mainConnective == ">" = max first second
    | otherwise             = modalAux (tail str) (n+1)
    where
      mainConnective = fst (mainC str)
      first          = modalAux (fst (split str)) n
      second         = modalAux (snd (split str)) n
