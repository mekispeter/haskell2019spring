{-
  Péter: Impressive as always! :)
  Some general suggestions:
  - Try not to overuse the auxiliary function technique.
  - Try to use more pattern matching whenever it results in a clearer and more
    readable code. If your definition has a lot of constructs like
    'head (tail ...)', most probably you need pattern matching.
-}

{-
  Péter: Here you're using strings as indexed arrays. Try not to! :)
  Here's a version without 'length' and '!!':
  least' :: (Ord a) => [a] -> a
  least' []     = error "sorry, empty list has no least element"
  least' (x:xs) = leastAux' x xs where
    leastAux' :: (Ord a) => a -> [a] -> a
    leastAux' x []     = x
    leastAux' x (y:ys)
      | y < x          = leastAux' y ys
      | otherwise      = leastAux' x ys
-}
-- give the least member
least :: Ord a => [a] -> a
least lst = leastAux lst 0 0
  where
    leastAux :: Ord a => [a] -> Int -> Int -> a
    leastAux lst i c
      | c == length lst = lst !! i
      | lst !! i <= lst !! c = leastAux lst i (c + 1)
      | otherwise = leastAux lst (i + 1) 0

{-
  Péter: You seem to be addicted to auxiliary functions! :) This case it just
  complicates things. Other than that, your algorithm fails when n is greater
  than the length of the list. Plus, the returned list is reversed.
  I'm not sure if these issues are bugs or features, but here's a version that
  avoids them:
  end' :: Int -> [a] -> [a]
  end' 0 lst    = []
  end' n []     = []
  end' n lst    =  end' (n-1) (init lst) ++ [last lst]
-}
-- take n element from the end of the list
end :: Int -> [a] -> [a]
end n lst = endAux 0 lst
  where
    endAux :: Int -> [a] -> [a]
    endAux c lst
      | n == 0 = []
      | c == n = []
      | otherwise = last lst : (endAux (c + 1) (init lst))

{-
  Very nice job! :) You can use (Ord a) => a instead of Int.
-}
-- bubblesort

{-
  Péter: If a function is not in a 'where' clause, you better cover all cases.
  Also, this idea is easier to express with pattern matching:
  swapBubble' :: [Int] -> [Int]
  swapBubble' []          = []
  swapBubble' (x:[])      = [x]
  swapBubble' (x:(y:lst))
    | y < x               = (y:(x:lst))
    | otherwise           = (x:(y:lst))
-}
-- swap first n m if n > m
swapBubble :: [Int] -> [Int]
swapBubble lst
  | head lst > head(tail lst) = [head(tail lst), head lst] ++ tail(tail lst)
  | otherwise                 = lst

{-
  Péter: Why do you ckeck the order of the first two items? This is what
  swapBubble is for, isn't it? Anyway, pattern matching might be a good idea
  here, too:
  bubbleAux' :: [Int] -> [Int]
  bubbleAux' [] = []
  bubbleAux' (x:[]) = [x]
  bubbleAux' (x:(y:lst))
    | y < x             = y:(bubbleAux' (x:lst))
    | otherwise         = x:(bubbleAux' (y:lst))
-}
-- swap all n m if n > m
bubbleAux :: [Int] -> [Int]
bubbleAux lst
  | length lst == 1            = [head lst]
  | head lst >= head(tail lst) = [head (swapBubble lst)] ++ bubbleAux (tail (swapBubble lst))
  | otherwise                  = [head lst] ++ bubbleAux (tail lst)

{-
  Péter: Why aux? :) You're not even using the counter! A version with pattern
  matching and without 'length':
  ordered' :: [Int] -> Bool
  ordered' []           = True
  ordered' (x:[])       = True
  ordered' (x:(y:lst))
    | y < x             = False
    | otherwise         = ordered' (y:lst)
-}
ordered :: [Int] -> Bool
ordered lst = aux lst 0
  where
    aux :: [Int] -> Int -> Bool
    aux lst counter
      | length lst == 1           = True
      | head lst < head(tail lst) = aux (tail lst) counter
      | otherwise                 = False

bubbleSort :: [Int] -> [Int]
bubbleSort lst
  | ordered lst = lst
  | otherwise   = bubbleSort (bubbleAux lst)

-- str1 has the same char as str2
same_elem :: String -> String -> Bool
same_elem [] (y:ys) = False
same_elem str [] = True
same_elem (x:xs) (y:ys) = if x `elem` (y:ys) then same_elem xs (y:ys) else False

same_char :: String -> String -> Bool
same_char str1 str2 = (same_elem str1 str2) && (same_elem str2 str1)

{-
  Péter: Beautiful! This approach is very different from the one taken in
  the sample.
-}

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * (fact (n-1))

n_k :: Int -> Int -> Int
n_k n k = fact n `div` (fact k * fact (n-k))

pas :: Int -> [Int]
pas n = pasAux n 0
  where
    pasAux :: Int -> Int -> [Int]
    pasAux n k
      | k == n =  [n_k n k]
      | otherwise = [n_k n k] ++ (pasAux n (k+1))

pascal :: Int -> [[Int]]
pascal n = pascalAux (n) 0
  where
    pascalAux :: Int -> Int -> [[Int]]
    pascalAux n c
      | (n-1) == c = [pas c]
      | otherwise = [pas c] ++ (pascalAux n (c+1))

{-
  Péter: Very nice! This is the basis of most parsing functions. Look for
  delimiters in a string, taking parenthesis depth into account.
-}
-- returns the main connective of a given formula with its index
mainC :: String -> (String, Int)
mainC str = mainAux str 0 0
  where
  mainAux :: String -> Int -> Int -> (String, Int)
  mainAux str n c
    | n == 1 && elem (head str) conn = ([head str], c)
    | head str == '(' = mainAux (tail str) (n+1) (c+1)
    | head str == ')' = mainAux (tail str) (n-1) (c+1)
    | otherwise = mainAux (tail str) n (c+1)
    where conn =['C','I','D']

{-
  I like that you avoided defining an auxiliary function here.
-}
substring :: String -> [String]
substring str = aux_substring str 0
  where
    aux_substring :: String -> Int -> [String]
    aux_substring str num
     | length str == 0 = [str]
     | num < length str = [[head str] ++ (take num (tail str))] ++ aux_substring str (num + 1)
     | otherwise = substring (tail str)
