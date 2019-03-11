{-
Ideas for the March 11 session.
-}

-- Integer lists:
-- EmptyI for []
-- Colon n lst for n:lst
data MyIntList = EmptyI | ColonI Integer MyIntList deriving (Read, Eq, Ord)

-- Instead of fimply deriving, we customize the 'show' function so that the
-- lists will look like lists.
instance Show MyIntList where
  show EmptyI = "[[]]"
  show (ColonI x EmptyI) = "[[" ++ (show x) ++ "]]"
  show (ColonI x xs) = take 2 (show xs) ++ (show x) ++ ";" ++ drop 2 (show xs)

-- examples
list1 :: MyIntList
list1 = ColonI 1 EmptyI
list2 :: MyIntList
list2 = ColonI 1 (ColonI 2 EmptyI)

-- implementing Python's range function
rangeInt :: Integer -> Integer -> MyIntList
rangeInt n m
  | m <= n    = EmptyI
  | otherwise = ColonI n (rangeInt (n+1) m)

-- binary tree structure with strings at the leafs and nodes
data MyStrTree = LeafS String | NodeS String MyStrTree MyStrTree deriving (Read, Eq, Ord)

-- customized show function
instance Show MyStrTree where
  show (LeafS s)       = s
  show (NodeS s t1 t2) = "<" ++ s ++ ":" ++ (show t1) ++ "," ++ (show t2) ++ ">"

-- examples
tree1 :: MyStrTree
tree1 = NodeS "S4" (LeafS "John") (NodeS "S5" (LeafS "love") (LeafS "Mary"))
tree2 :: MyStrTree
tree2 = NodeS "+" (LeafS "1") (NodeS "*" (LeafS "2") (LeafS "3"))

-- MyIntList generalized: now a list of any type of data
data MyList a = Empty | Colon a (MyList a) deriving (Eq, Ord)

-- Show customized: works only if a is an instance of Show
instance (Show a) => Show (MyList a) where
  show Empty = "[[]]"
  show (Colon x Empty) = "[[" ++ (show x) ++ "]]"
  show (Colon x xs) = take 2 (show xs) ++ (show x) ++ ";" ++ drop 2 (show xs)

-- Instantiation of Read is complicated; instead we go with
-- Specify 'a' when calling; eg. > myRead "[[1;2;11]]" :: MyList Integer
myRead :: (Read a) => String -> MyList a
myRead "[[]]" = Empty
myRead s      = getColon "" (tail (tail s)) where
  getColon :: (Read a) => String -> String -> MyList a
  getColon sL sR
    | sR == ""        = error "ill-formed MyList string"
    | sR == "]]"      = Colon (read sL) Empty
    | head sR == ';'  = Colon (read sL) (myRead ("[[" ++ (tail sR)))
    | otherwise       = getColon (sL ++ [head sR]) (tail sR)

-- examples
list3 :: MyList Char
list3 = Colon  'a' Empty
list4 :: MyList Integer
list4 = Colon 1 (Colon 2 Empty)
lPlus :: MyList a -> MyList a -> MyList a
lPlus Empty ml           = ml
lPlus (Colon x ml1) ml2  = Colon x (lPlus ml1 ml2)
-- [[True],[False]]
--truthTableL :: Integer -> MyList (MyList Bool)
--truthTableL 1 = Colon (Colon True Empty) (Colon (Colon False Empty) Empty)
--otherwise

-- range reconsidered: now it works for every Ord type
range :: (Ord a, Enum a) => a -> a -> MyList a
range x y
  | y <= x    = Empty
  | otherwise = Colon x (range (succ x) y)

-- MyStrTree generalized: type a data at leaves, type b data at nodes
data MyTree a b = Leaf a | Node b (MyTree a b) (MyTree a b) deriving (Read, Eq, Ord)

-- Show works if both a and b are instances of the Show class.
instance (Show a, Show b) => Show (MyTree a b) where
  show (Leaf x)       = show x
  show (Node x t1 t2) = "<" ++ (show x) ++ ":" ++ (show t1) ++ "," ++ (show t2) ++ ">"

-- examples
tree3 :: MyTree String String
tree3 = Node "S4" (Leaf "John") (Node "S5" (Leaf "love") (Leaf "Mary"))
tree4 :: MyTree Integer String
tree4 = Node "+" (Leaf 1) (Node "*" (Leaf 2) (Leaf 3))

-- this function generates truthtables as trees
truthTableT :: Integer -> (MyTree [Bool] Integer)
truthTableT 1 = Node 1 (Leaf [True]) (Leaf [False])
truthTableT n = expand n (truthTableT (n-1)) where
  expand :: Integer -> (MyTree [Bool] Integer) -> (MyTree [Bool] Integer)
  expand n (Leaf lst) = Node n (Leaf (True:lst)) (Leaf (False:lst))
  expand n (Node m tree1 tree2) = Node m (expand n tree1) (expand n tree2)

leavesFromTree :: MyTree a b -> [a]
leavesFromTree (Leaf x)             = [x]
leavesFromTree (Node y tree1 tree2) = leavesFromTree tree1 ++ leavesFromTree tree2

nodesFromTree :: MyTree a b -> [b]
nodesFromTree (Leaf x)              = []
nodesFromTree (Node y tree1 tree2)  = y : (nodesFromTree tree1 ++ nodesFromTree tree2)

parse :: String -> String -> MyTree String Char
parse s delimiters
  | hasOuterPar s             = parse (init (tail s)) delimiters
  | otherwise                 = parseAux "" s delimiters 0
  where
    parseAux :: String -> String -> String -> Integer -> MyTree String Char
    parseAux sL "" ds n       = Leaf sL
    parseAux sL (c:sR) ds n
      | c == '('              = parseAux (sL++[c]) sR ds (n+1)
      | c == ')'              = parseAux (sL++[c]) sR ds (n-1)
      | c `elem` ds && n == 0 = Node c (parse sL ds) (parse sR ds)
      | otherwise             = parseAux (sL++[c]) sR ds n
    hasOuterPar :: String -> Bool
    hasOuterPar s
      | s == ""       = False
      | head s /= '(' = False
      | last s /= ')' = False
      | otherwise = closesAtLast 1 (tail s)
    closesAtLast :: Integer -> String -> Bool
    closesAtLast n s
      | n == 0 && s == "" = True
      | s == []           = False
      | n == 0            = False
      | head s == '('     = closesAtLast (n+1) (tail s)
      | head s == ')'     = closesAtLast (n-1) (tail s)
      | otherwise         = closesAtLast n (tail s)

logForm1 = "((pCq)Ip)"
logForm2 = "(((pCq)Ir)C(pIq))I(pIr)"
logOps = "CDIB"
arTerm1 = "s+1/2^n"
arTerm2 = "s+(1/2^n)"
arOps = "+-*/^"

data MyTree2 a b  = Node0 a |
                    Node1 b (MyTree2 a b) |
                    Node2 b (MyTree2 a b) (MyTree2 a b)
                    deriving (Read, Eq, Ord)

instance (Show a, Show b) => Show (MyTree2 a b) where
  show (Node0 x)        = show x
  show (Node1 x t)      = "(" ++ (show x) ++ ":" ++ (show t) ++ ")"
  show (Node2 x t1 t2)  = "(" ++ (show x) ++ ":"
                          ++ (show t1) ++ "," ++ (show t2) ++ ")"

parse2 :: String -> String -> String -> MyTree2 String Char
parse2 s delim1 delim2
  | s == ""                   = Node0 s
  | hasOuterPar s             = parseAux "" (init (tail s)) delim1 delim2 0
  | (head s) `elem` delim1    = Node1 (head s) (parse2 (tail s) delim1 delim2)
  | otherwise                 = Node0 s
  where
    parseAux :: String -> String -> String -> String -> Integer
                -> MyTree2 String Char
    parseAux sL "" d1 d2 n    = parse2 sL d1 d2
    parseAux sL (c:sR) d1 d2 n
      | c == '('              = parseAux (sL++[c]) sR d1 d2 (n+1)
      | c == ')'              = parseAux (sL++[c]) sR d1 d2 (n-1)
      | c `elem` d2 && n == 0 = Node2 c (parse2 sL d1 d2) (parse2 sR d1 d2)
      | otherwise             = parseAux (sL++[c]) sR d1 d2 n
    hasOuterPar :: String -> Bool
    hasOuterPar s
      | s == ""       = False
      | head s /= '(' = False
      | last s /= ')' = False
      | otherwise = closesAtLast 1 (tail s)
    closesAtLast :: Integer -> String -> Bool
    closesAtLast n s
      | n == 0 && s == "" = True
      | s == []           = False
      | n == 0            = False
      | head s == '('     = closesAtLast (n+1) (tail s)
      | head s == ')'     = closesAtLast (n-1) (tail s)
      | otherwise         = closesAtLast n (tail s)

logForm3 = "(N(pIq)B(NqINp))"
logForm4 = "(N(NpCNq)B(pDq))"
logOps1 = "N"
logOps2 = "CDIB"
arTerm3 = "s+2^mn"
arOps1 = "m"
arOps2 = "+-*/^"
