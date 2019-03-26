{-
  Functional programming for logicians
  2019 March 25

  Types and type classes continued: functors and foldables
-}

{-
  We reconstruct the list type. 'Empty' is for '[]', and 'Colon' is for ':'.
  We derive from the type classes that are not interesting right now.
-}
data List a = Empty | Colon a (List a)
              deriving (Show, Read, Eq, Ord)
{-
  Eg. 'Colon 1 (Colon 2 (Colon 3 Empty))' will be our version of '[1,2,3]',
  just as '[1,2,3]' itself is constructed as '1 : (2 : (3 : []))'.
-}

{-
  Mapping is a very useful tool to handle values in the context of a list,
  without removing them from that context.
-}
lMap :: (a -> b) -> List a -> List b
lMap f Empty        = Empty
lMap f (Colon x xs) = Colon (f x) (lMap f xs)
{-
  Eg.
  > lMap (^2) (Colon 1 (Colon 2 (Colon 3 Empty)))
  Colon 1 (Colon 4 (Colon 9 Empty))
-}

{-
  Mapping is a very general pattern. It occurs in a lot of types, recursive or
  otherwise, where values in a specific context are handled without changing
  the context. To avoid ad hoc definitions with ad hoc names, there is a type
  class for types that support mapping. It is called the 'Functor' class, and
  its main method is 'fmap', an abstract polymorphic version of 'map' for all
  instances of Functor.

  The type of fmap is defined in the class definition, but it is good to have
  it here:
  fmap :: (Functor f) => (a -> b) -> f a -> f b
-}
instance Functor List where
  fmap f Empty        = Empty
  fmap f (Colon x xs) = Colon (f x) (fmap f xs)

{-
  Let us return to the binary tree type defined the last week.
-}
data Tree a = Leaf a | Node a (Tree a) (Tree a)
              deriving (Show, Read, Eq)

{-
  Let's see how we can instantiate Functor.
-}
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x t1 t2) = Node (f x)
                          (fmap f t1) (fmap f t2)

{-
  We copy a tree here to make calls from ghci easier. Eg.
  > fmap (*2) numberTree
  Node 46 (Leaf 6) (Node 40 (Leaf 8) (Leaf 10))
  > fmap (show) numberTree
  Node "23" (Leaf "3") (Node "20" (Leaf "4") (Leaf "5"))
-}
numberTree  :: Tree Integer
numberTree =    Node 23
                (Leaf 3)
                (Node 20 (Leaf 4) (Leaf 5))

{-
  A homework fromlast week: another version of the binary tree.
-}
data Tree1 a = Empty1 | Node1 a (Tree1 a) (Tree1 a)
               deriving (Show, Read, Eq)

{-
  Let's implement Functor once again.
-}
instance Functor Tree1 where
  fmap f Empty1 = Empty1
  fmap f (Node1 x t1 t2) = Node1 (f x)
                           (fmap f t1) (fmap f t2)

{-
  The sample tree changes with the definition, but fmap works the same way:
  > fmap (*2) numberTree1
  Node1 46 (Node1 6 Empty1 Empty1) (Node1 40 (Node1 8 Empty1 Empty1)
  (Node1 10 Empty1 Empty1))
  > fmap (show) numberTree1
  Node1 "23" (Node1 "3" Empty1 Empty1) (Node1 "20" (Node1 "4" Empty1 Empty1)
  (Node1 "5" Empty1 Empty1))
-}
numberTree1 :: Tree1 Integer
numberTree1 =    Node1 23
                (Node1 3 Empty1 Empty1)
                (Node1 20 (Node1 4 Empty1 Empty1) (Node1 5 Empty1 Empty1))

{-
  Finally, we copy the Hunmaybe type from last week to see a non-recursive
  example.
-}
data HunMaybe a = Semmi | Csak a
                  deriving (Show, Read, Eq)

{-
  As it turns out, fmap is not necessarily recursive. It is recursive on
  recursive datatypes, and non-recursive on non-recursive ones. Calls of fmap
  work as expected, eg.
  > fmap (^2) (Csak 3)
  Csak 9
  > (fmap . fmap) (^2) (Colon (Csak 3) (Colon (Csak 4) (Colon (Csak 5) Empty)))
  Colon (Csak 9) (Colon (Csak 16) (Colon (Csak 25) Empty))
-}
instance Functor HunMaybe where
  fmap f Semmi    = Semmi
  fmap f (Csak x) = Csak (f x)

{-
  Another method frequently occurring with lists is processing elements one by
  one with a binary function, thus folding up the list, and removing the result
  from its original context.
-}
lFoldR :: (a -> b -> b) -> b -> (List a) -> b
lFoldR f x Empty        = x
lFoldR f x (Colon y ys) = f y (lFoldR f x ys)
{-
  Eg.
  > lFoldR (*) 6 (Colon 1 (Colon 2 (Colon 3 (Colon 4 Empty))))
  144
  -- ie. 1 * (2 * (3 * (4 * 6)))
-}

{-
This, again, is an instance of a very general abstract pattern, used for a lot
of tasks, from summing to deciding elementhood. To avoid ad hoc
definitions like the one above, it comes together with its typeclass.
-}
instance Foldable List where
  foldr f x Empty        = x
  foldr f x (Colon y ys) = f y (foldr f x ys)
{-
  Defining 'foldr' will activate a lot of methods for the type class, all
  defined in terms of 'foldr'; including 'elem', 'length', 'sum', and many
  others. For the full list, type ':i Foldable' in ghci.
  Eg.
  > elem 3 (Colon 1 (Colon 2 (Colon 3 (Colon 4 Empty))))
  True
  > sum (Colon 1 (Colon 2 (Colon 3 (Colon 4 Empty))))
  10
  length (Colon 1 (Colon 2 (Colon 3 (Colon 4 Empty))))
                                             4
-}
