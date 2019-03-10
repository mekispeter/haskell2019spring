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
