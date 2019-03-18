{-
  Functional programming for logicians
  2019 March 18

  Last time we defined a few types from existing ones. Now we start defining
  types from scratch, using data constructors.
-}

{-
  HunBool is a duplicate of the Bool type. We use Hungarian and Spanish words
  for fun. 'Hamis' and 'Igaz' are type constructors without parameters.
  In fact, they are the only two values of this type.
  HunBool is an instance of several type classes:
  - Eq: the class of types for which equality is defined
    methods: (==), (/=)
  - Ord: the class of orderable types
    methods: (<), (<=), etc.
  - Read: the class of types the values of which can be read from strings
    methods: readsPrec etc. (not the read function that we know)
  - Enum: the class of enumerable types
    methods: succ, pred, etc.
  - Bounded: the class of bounded types
    methods: minBound, maxBound
-}
data HunBool = Hamis | Igaz deriving (Ord, Read, Enum, Bounded)

-- We could just derive Eq and Show, too, but it's fancier to define them:
instance Eq HunBool where
  (==) Igaz Igaz   = True
  (==) Hamis Hamis = True
  (==) x y         = False

instance Show HunBool where
  show Igaz   = "Verdad"
  show Hamis  = "Falso"

-- We can now redefine any function and any type involving Bool:
type Relation a = a -> a-> Bool
type HunRelation a = a -> a-> HunBool

{-
  HunMaybe is a duplicate of the Maybe type. This type is used to handle
  partial functions. The case where is no value is handled with the 'Nothing'
  value ('Semmi' in our version); and the cases where there is a value x is
  handled with the 'Just x' case ('Csak x' in our version.)
  Maybe is a simple but extremely useful type. It is at the heart of a lot of
  Haskell tricks.
-}
data HunMaybe a = Semmi | Csak a
-- Nothing | Just a

-- Again, we could derive Show, but it's funnier to manually define it. Note
-- that the wsowability of 'Just a' is inherited from the showability of 'a'.
instance (Show a) => Show (HunMaybe a) where
  show Semmi    = "Nada"
  show (Csak a) = "Solo " ++ show a

-- A partial function: no return value is defined for 4. The call
-- '> sok 4' raises an error message.
sok :: Integer -> HunBool
sok 0 = Hamis
sok 1 = Hamis
sok 2 = Hamis
sok 3 = Igaz

-- The same function with built-in exception handling: Whenever there's no
-- value, the value will be 'Nothing' (in our version, 'Semmi'). In the cases
-- where there is a value, it is whrapped in a 'Just' (in our version, 'Csak').
sok' :: Integer -> HunMaybe HunBool
sok' 0 = Csak Hamis
sok' 1 = Csak Hamis
sok' 2 = Csak Hamis
sok' 3 = Csak Igaz
sok' n = Semmi

{-
  And now the real fun: recursive data types. Let's start with a simple
  implementation of binary trees. In this version every node has two children,
  and nodes and leaves are labelled with the same type of data. Later we can
  introduce more sophisticated versions of the Tree type.
-}
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
-- Note the recursive nature of the data constructor 'Node': it has two Tree
-- parameters along with the label parameter.

-- An instance from syntax:
montagueTree :: Tree String
montagueTree =  Node "S4"
                (Leaf "John")
                (Node "S5" (Leaf "love") (Leaf "Mary"))
-- S4
    -- John
    -- S5
        -- love
        -- Mary

-- A similar construction with numbers:
numberTree  :: Tree Integer
numberTree =    Node 23
                (Leaf 3)
                (Node 20 (Leaf 4) (Leaf 5))
-- 23
    -- 3
    -- 20
        -- 4
        -- 5

-- Let's adapt the concept of length from lists:
treeLength :: Tree a -> Integer
treeLength (Leaf x)       = 1
treeLength (Node x t1 t2) = 1 + treeLength t1 + treeLength t2

{-
  Lists and trees share two very important features:
  - Both of them can be processed by a recursive algorithm; they are instances
    of the Foldable type class. (See the definition of treeLength above.)
  - Both of them can be mapped over; they are instances of the Functor type
    class. (Eg. mapping (*2) will double the labels at the nodes and leaves of
    a Tree Integer structure like numberTree above.)

  In the next session we will discuss these types in detail.
-}
