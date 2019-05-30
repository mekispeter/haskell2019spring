{-
  Functional Programming for Logicians 2019 Spring
  Project 2: Game of Life

  This program implements Conway's game of life.
  Patterns are read from files, and each generation is printed in the console.
  Call 'main' to start a simmulation. Call 'test' to check the integrity
  of some core functions (comes handy when you change them).

  If the unicode box drawing characters are not displayed properly,
  switch from 'fancyShowPattern' to 'showPattern' in the definition
  of 'displayPattern'.

  The program consists of the following parts:
  - Main IO loop
  - Definition of a grid
  - A simple show method for grids
  - Grid parsing
  - Grid updating
  - A fancy show method for grids
  - A test function
-}

import qualified Data.Vector as V
import Data.Vector ((!))

{-
  Main loop with all the IO actions
-}

main :: IO ()
main = do
  putStrLn "Pattern:"
  patternName       <- getLine
  patternString     <- readFile $ patternName ++ ".grid"
  let pattern       = readPattern patternString
  putStrLn "Generations:"
  genString         <- getLine
  let generations   = read genString :: Int
  updateLoop pattern generations generations patternName

updateLoop :: Grid -> Int -> Int -> String -> IO ()
updateLoop pattern 0 m patternName =
  displayPattern pattern m patternName
updateLoop pattern n m patternName = do
  displayPattern pattern (m - n) patternName
  _ <- getLine
  updateLoop (updatePattern pattern) (n-1) m patternName

displayPattern :: Grid -> Int -> String -> IO ()
displayPattern pattern gen patternName = do
  putStrLn $ "Pattern " ++ patternName ++ ", generation " ++ show gen
  --putStrLn $ showPattern pattern
  putStrLn $ fancyShowPattern pattern

{-
  A cell has two states: dead or alive. A grid is a two-cimensional
  matrix of cells, implemented as an embedded vector.
-}

data CellState      = Dead | Alive deriving (Eq, Ord, Enum)
type Grid           = V.Vector (V.Vector CellState)

gridSizeY :: Grid -> Int
gridSizeY pattern   = V.length pattern
gridSizeX :: Grid -> Int
gridSizeX pattern
  | gridSizeY pattern == 0  = 0
  | otherwise               = V.length $ V.head pattern

isEmpty :: Grid -> Bool
isEmpty pattern = V.null pattern || V.null (V.head pattern)

getCell :: Grid -> Int -> Int -> CellState
getCell pattern x y = pattern ! (y `mod` gridSizeY pattern)
                              ! (x `mod` gridSizeX pattern)

{-
  The simpler showPattern method: use this if the other one fails.
-}

instance Show CellState where
  show Dead         = "."
  show Alive        = "#"

showPattern :: Grid -> String
showPattern pattern = foldr concatLine ""
                      (fmap (concat . V.toList) $ doubleMap show pattern) where

-- also used by fancyShowPattern
concatLine :: String -> String -> String
concatLine s z      = s ++ '\n':z

-- also used elsewhere
doubleMap :: Functor c => (a -> b) -> c (c a) -> c (c b)
doubleMap           = fmap . fmap

{-
  Parsing patterns from strings. Rows are lines of the string, and cells are single characters: '.' for dead cells, and '#' (or anything else)
  for alive ones.
-}

readPattern :: String -> Grid
readPattern s       = patternFromList $ doubleMap readCell (lines s)

readCell :: Char -> CellState
readCell '.'        = Dead
readCell _          = Alive

patternFromList :: [[CellState]] -> Grid
patternFromList xss = V.fromList (map V.fromList $ xss)

{-
Updating a grid follows the neighborhood concept and update rool chosen by
Comway. These can be freely altered without affecting the rest of the program.
- One option is switching from Moore neighborhood to Neumann neighborhood,
  which doesn't contain diagonal neighbors:
  neumannNeighborhood = [(0, -1), (-1,  0), (1,  0), (0,  1)]
- Another option is choosing different criteria in the updateCell function.
- These options can also be parametrized, making the program capable of
  implementing a wide range of cellular automata.
-}

neighborhood = [(-1, -1), (0, -1),  (1, -1),
                (-1,  0),           (1,  0),
                (-1,  1), (0,  1),  (1,  1)]

aliveNeighbors :: Grid -> Int -> Int -> Int
aliveNeighbors pattern x y  = length $
                              filter (== Alive)
                              [getCell pattern (x+i) (y+j) |
                              (i,j) <- neighborhood]

updatePattern :: Grid -> Grid
updatePattern pattern
  | isEmpty pattern = pattern
  | otherwise       = patternFromList $
                      [
                        [updateCell pattern x y | x <- [0..maxX]]
                        | y <- [0..maxY]
                      ]
  where
    maxX = gridSizeX pattern - 1
    maxY = gridSizeY pattern - 1

updateCell :: Grid -> Int -> Int -> CellState
updateCell pattern x y
  | readyToBeBorn   = Alive
  | isWell          = Alive
  | otherwise       = Dead
  where
     readyToBeBorn  = self == Dead && othersAlive == 3
     isWell         = self == Alive && (othersAlive == 2 || othersAlive == 3)
     self           = getCell pattern x y
     othersAlive    = aliveNeighbors pattern x y

{-
  A fancier way of displaying the grids. There's a chance some of the
  characters will not display correctly in every environment.
-}

fancyChars :: String
fancyChars =   " ▗▖▄▝▐▞▟▘▚▌▙▀▜▙█"
fancyQuarter :: (CellState, CellState, CellState, CellState) -> Char
fancyQuarter (ul, ur, bl, br) = fancyChars !!
                                (8*(i ul) + 4*(i ur) + 2*(i bl) + i br)
                                where
                                  i = fromEnum

getQuarter :: Grid -> Int -> Int ->
              (CellState, CellState, CellState, CellState)
getQuarter pattern x y    = (getCell pattern (2*x) (2*y),
                            getCell pattern (2*x+1) (2*y),
                            getCell pattern (2*x) (2*y+1),
                            getCell pattern (2*x+1) (2*y+1))

fancyShowAsList :: Grid -> [String]
fancyShowAsList pattern
  | isEmpty pattern = [""]
  | otherwise             = [
                              [fancyQuarter (getQuarter pattern x y)
                                | x <- [0..maxX]]
                              | y <- [0..maxY]
                            ]
                            where
                              maxX = (gridSizeX pattern - 1) `div` 2
                              maxY = (gridSizeY pattern - 1) `div` 2

fancyShowAddBorder :: [String] -> [String]
fancyShowAddBorder xss = [topLine] ++ map lineEndings xss ++ [bottomLine] where
  topLine = "┏" ++ replicate (length $ head xss) '━' ++ "┓"
  lineEndings xs = "┃" ++ xs ++ "┃"
  bottomLine = "┗" ++ replicate (length $ head xss) '━' ++ "┛"


fancyShowPattern :: Grid -> String
fancyShowPattern pattern = foldl concatLine ""
                           (fancyShowAddBorder $ fancyShowAsList pattern)

{-
  This is a test that checks whether the core functions work correctly.
  It comes handy when these are modified.
-}
test :: IO ()
test = do
        patternString <- readFile "pulsar.grid"
        let pattern = readPattern patternString
        let pattern' = (updatePattern . updatePattern . updatePattern) pattern
        if  gridSizeX pattern1 == 5 &&
            gridSizeY pattern1 == 6
          then putStrLn "- gridSize             OK"
          else putStrLn "- gridSize             not OK"
        if  aliveNeighbors pattern1 (-3) 9 == 6 &&
            aliveNeighbors pattern1   0  1 == 1
          then putStrLn "- aliveNeighbors       OK"
          else putStrLn "- aliveNeighbors       not OK"
        if  updateCell pattern1 1 1   == Alive &&
            updateCell pattern1 11 18 == Dead
          then putStrLn "- updateCell           OK"
          else putStrLn "- updateCell           not OK"
        if  updatePattern pattern1 == pattern2 &&
            pattern == pattern'
          then putStrLn "- updatePattern        OK"
          else putStrLn "- updatePattern        not OK"
        if  showPattern pattern1 == patternString1 ++ "\n"
          then putStrLn "- showPattern          OK"
          else putStrLn "- showPattern          not OK"
        if  fancyShowPattern pattern1 == fSpattern1
          then putStrLn "- fancyShowPattern     OK"
          else putStrLn "- fancyShowPattern     not OK"

patternString1  = ".....\n..#..\n.###.\n.###.\n..#..\n....."
pattern1        = readPattern patternString1
fSpattern1      = "\n\9487\9473\9473\9473\9491\n\9475 \9622 \9475\
                  \\n\9475\9616\9608 \9475\n\9475 \9624 \9475\
                  \\n\9495\9473\9473\9473\9499"
patternString2  = ".....\n.###.\n.....\n.....\n.###.\n....."
pattern2        = readPattern patternString2
