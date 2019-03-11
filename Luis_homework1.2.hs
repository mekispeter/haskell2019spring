{-
  Péter's comments:

  It won't compile this way:
  - Function names have to start with a smallcase letter, eg. 'andfunction' or
    'andFunction' instead of 'Andfunction',
  - Haskell's built-in Boolean values are 'True' and 'False', not 'TRUE' and
    'FALSE'.
  Other than these problems, the definitions are correct.
-}

Andfunction :: Bool-> (Bool->Bool)
Andfunction n m
|n == FALSE = FALSE
|m == FALSE = FALSE
|otherwise = TRUE

Orfunction :: Bool-> (Bool->Bool)
Orfunction n m
| n==TRUE = TRUE
| m==TRUE = TRUE
| otherwise FALSE
