{-
A few sorting algorithms expressed as elementary recursions. No fancy stuff
is used, except for list comprehension in one case.
-}

-- Quicksort 1: without list comprehension
-- https://en.wikipedia.org/wiki/Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort lst
  | null lst    = lst
  | otherwise   = quicksort (smaller h (tail lst)) ++ h : quicksort (larger h (tail lst))
  where
    h = head lst
    smaller :: (Ord a) => a -> [a] -> [a]
    smaller x s
      | null s      = s
      | head s < x  = head s : smaller x (tail s)
      | otherwise   = smaller x (tail s)
    larger :: (Ord a) => a -> [a] -> [a]
    larger x s
      | null s      = s
      | x <= head s = head s : larger x (tail s)
      | otherwise   = larger x (tail s)

--- Quicksort 2: with list comprehension.
quicksort' :: (Ord a) => [a] -> [a]
quicksort' lst
  | null lst    = lst
  | otherwise   = quicksort' smaller ++ h : quicksort' larger
  where
    h = head lst
    smaller = [x | x <- tail lst, x < h]
    larger = [x | x <- tail lst, h <= x]

-- Selection sort
-- https://en.wikipedia.org/wiki/Selection_sort
selectionsort :: (Ord a) => [a] -> [a]
selectionsort lst
  | null lst    = lst
  | otherwise   = minimum lst : selectionsort (remove (minimum lst) lst)
  where
    remove :: (Ord a) => a -> [a] -> [a]
    remove x lst
      | null lst        = lst
      | x == head lst   = tail lst
      | otherwise       = head lst : remove x (tail lst)

-- Insertion sort
-- https://en.wikipedia.org/wiki/Insertion_sort
insertionsort :: (Ord a) => [a] -> [a]
insertionsort lst = insertionaux [] lst
  where
    insertionaux :: (Ord a) => [a] -> [a] -> [a]
    insertionaux ready remains
      | null remains    = ready
      | otherwise       = insertionaux (insert (head remains) ready) (tail remains)
      where
        insert :: (Ord a) => a -> [a] -> [a]
        insert x lst
          | null lst      = [x]
          | x < head lst  = x:lst
          | otherwise     = head lst : insert x (tail lst)

-- Bubble sort
-- https://en.wikipedia.org/wiki/Bubble_sort
bubblesort :: (Ord a) => [a] -> [a]
bubblesort lst = bubble_aux [] lst False
  where
    bubble_aux :: (Ord a) => [a] -> [a] -> Bool -> [a]
    bubble_aux ready remains swapped
      | null remains && swapped == False = ready
      | null remains                     = bubble_aux [] ready False
      | null ready = bubble_aux [head remains] (tail remains) False
      | head remains < last ready = bubble_aux (init ready ++ [head remains] ++ [last ready]) (tail remains) True
      | otherwise = bubble_aux (ready ++ [head remains]) (tail remains) swapped
