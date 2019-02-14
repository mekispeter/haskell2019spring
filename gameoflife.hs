{-
A very simple implementation of Conway's Game of life in Haskell. Populations
are embedded lists. Living cells are displayed as '#', dead cells by ' '.
Load it in ghci, and call gol, eg:
-- > gol (read_grid sample3) 10
-}

-- types:
-- Cell is basically a Boolean
-- cell rows are lists of cells
-- cell grids are lists of cell rows
-- probably two dimensional arrays would be a better choice instead of lists,
-- since we need to use indices
data Cell = Dead | Alive deriving Eq
type Cellrow = [Cell]
type Cellgrid = [[Cell]]

-- cell from its char representation
char_to_cell :: Char -> Cell
char_to_cell x
  | x == ' '  = Dead
  | otherwise = Alive

-- char representation from cell
cell_to_char :: Cell -> Char
cell_to_char c
  | c == Dead = ' '
  | otherwise = '#'

-- samples are given as lists of strings below
read_grid :: [String] -> Cellgrid
read_grid sl = [map char_to_cell s | s <- sl]

-- grids are printed as strings; "\n" is line break
show_grid :: Cellgrid -> String
show_grid g = concat [map cell_to_char r ++ "\n" | r <- g]

-- checks how many neighbor cells are alive
checkneighb :: Cellgrid -> (Int, Int) -> Int
checkneighb g (n,m) = length [p | p <- neighb, on_grid p, get_cell p == Alive] where
  neighb :: [(Int, Int)]
  neighb = [(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)]
  get_cell p = (g!!(mc p))!!(nc p)
  on_grid p = 0 <= nc p && nc p < nsize && 0 <= mc p && mc p < msize
  nsize = length (head g)
  msize = length g
  nc p = n + fst p
  mc p = m + snd p

-- gets the new value of a cell of a population based on the previous
-- state of the population
newvalue_cell :: Cellgrid -> (Int, Int) -> Cell
newvalue_cell g (n,m)
  | cell == Dead && score == 3  = Alive --dead cells with exactly 3 alive neighbors become alive
  | cell == Dead                = Dead -- other dead cells stay dead
  | score < 2 || 3 < score      = Dead -- alive cells with too few or too many neighbors die
  | otherwise                   = Alive -- other alive cells stay alive
  where
    score = checkneighb g (n,m)
    cell = (g!!m)!!n

-- updates a grid of cells
update :: Cellgrid -> Cellgrid
update g = [[newvalue_cell g (n,m) | n <- n_i] | m <- m_i] where
  n_i = [0..length (head g) - 1]
  m_i = [0..length g - 1]

-- updates a population n times, and prints the result
-- you can animate the sample by incrementing the second argument
-- in repeated calls
gol :: Cellgrid -> Int -> IO ()
gol g n
  | n == 0    = putStrLn(show_grid g)
  | otherwise = gol (update g) (n-1)

-- this part is not written yet; 'main' should be interactive
main = gol (read_grid (sample4)) 1000

-- a few patterns:
samples = [sample0,sample1,sample2,sample3,sample4]

-- 5x5
sample0 :: [[Char]]
sample0 = ["     ",
           " # # ",
           "  #  ",
           " # # ",
           "     "]

-- 10x10
sample1 :: [[Char]]
sample1 = ["          ",
           "   #  #   ",
           "   #  #   ",
           " ## ## ## ",
           "   #  #   ",
           "   #  #   ",
           " ## ## ## ",
           "   #  #   ",
           "   #  #   ",
           "          "]

-- 17x17
sample2 :: [[Char]]
sample2 = ["                 ",
           "        #        ",
           "  #      #       ",
           "   #   ###       ",
           " ###             ",
           "                 ",
           "                 ",
           "           #     ",
           "           #     ",
           "           #     ",
           "                 ",
           "       ###   ### ",
           "                 ",
           "           #     ",
           "           #     ",
           "           #     ",
           "                 "]

-- 34x8
sample3 :: [[Char]]
sample3  = ["                                  ",
            "                                  ",
            "                            ####  ",
            "                           ###### ",
            "                          ## #### ",
            "                           ##     ",
            "                                  ",
            "                                  "]

-- 47x24
sample4 :: [[Char]]
sample4  = ["                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                   ### #       ",
            "                                   #           ",
            "                                      ##       ",
            "                                    ## #       ",
            "                                   # # #       ",
            "                                               ",
            "                                               ",
            "                                               ",
            "                                               "]
