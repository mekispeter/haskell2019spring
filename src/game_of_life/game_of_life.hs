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
  - Main IO stuff
  - Definitions of cell and pattern
  - Pattern parsing
  - Pattern updating
  - A test function
-}

import Grid
import Codec.Picture

{-
  Main loop with all the IO actions
-}

main :: IO ()
main = do
  putStrLn "Pattern:"
  patternName       <- getLine
  patternString     <- readFile $ "gol_patterns/" ++ patternName ++ ".grid"
  let pattern       = readPattern patternString
  putStrLn "Generations:"
  genString         <- getLine
  let generations   = read genString :: Int
  putStrLn "Display mode (1: fancy, 2: simple, 3: gif):"
  displayModeString <- getLine
  let displayMode   = read displayModeString :: Int
  if displayMode < 3
    then updateLoop pattern generations generations patternName displayMode
    else do
      writeToGif patternName $ updateToList pattern generations
      putStrLn $ "Animation exported to " ++ patternName ++ ".gif."

updateLoop :: Pattern -> Int -> Int -> String -> Int ->
              IO ()
updateLoop pattern 0 m patternName displayMode =
  displayPattern pattern m patternName displayMode
updateLoop pattern n m patternName displayMode = do
  displayPattern pattern (m - n) patternName displayMode
  _ <- getLine
  updateLoop (updatePattern pattern) (n-1) m patternName displayMode

displayPattern :: Pattern -> Int -> String -> Int -> IO ()
displayPattern pattern gen patternName displayMode = do
  putStrLn $ "Pattern " ++ patternName ++ ", generation " ++ show gen
  if displayMode == 1
    then putStrLn $ showGridFancy pattern (== Alive)
    else putStrLn $ showGridSimple pattern ""

writeToGif :: String -> [Pattern] -> IO()
writeToGif name gridList = writeGridsToGif gridList ("gol_images/" ++ name)
  renderPixel 8
  where
    renderPixel Alive = PixelRGB8 196 30 61
    renderPixel Dead  = PixelRGB8 245 245 245

{-
  A cell has two states: dead or alive. A grid is a two-cimensional
  matrix of cells, implemented as an embedded vector.
-}

data CellState      = Dead | Alive deriving (Eq, Ord, Enum)
type Pattern        = Grid CellState

instance Show CellState where
  show Dead         = "."
  show Alive        = "#"

getCell :: Pattern -> Int -> Int -> CellState
getCell pattern x y = pattern !!! (modX, modY) where
  modX = x `mod` gridSizeX pattern
  modY = y `mod` gridSizeY pattern

-- also used elsewhere
doubleMap :: Functor c => (a -> b) -> c (c a) -> c (c b)
doubleMap           = fmap . fmap

{-
  Parsing patterns from strings. Rows are lines of the string, and cells are single characters: '.' for dead cells, and '#' (or anything else)
  for alive ones.
-}

readPattern :: String -> Pattern
readPattern s       = gridFromList $ doubleMap readCell (lines s)

readCell :: Char -> CellState
readCell '.'        = Dead
readCell _          = Alive

{-
Updating a grid follows the neighborhood concept and update rule chosen by
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

aliveNeighbors :: Pattern -> Int -> Int -> Int
aliveNeighbors pattern x y  = length $
                              filter (== Alive)
                              [getCell pattern (x+i) (y+j) |
                              (i,j) <- neighborhood]

updatePattern :: Pattern -> Pattern
updatePattern pattern
  | isEmpty pattern = pattern
  | otherwise       = gridFromList $
                      [
                        [updateCell pattern x y | x <- [0..maxX]]
                        | y <- [0..maxY]
                      ]
  where
    maxX = gridSizeX pattern - 1
    maxY = gridSizeY pattern - 1

updateCell :: Pattern -> Int -> Int -> CellState
updateCell pattern x y
  | readyToBeBorn   = Alive
  | isWell          = Alive
  | otherwise       = Dead
  where
     readyToBeBorn  = self == Dead && othersAlive == 3
     isWell         = self == Alive && (othersAlive == 2 || othersAlive == 3)
     self           = getCell pattern x y
     othersAlive    = aliveNeighbors pattern x y

updateToList :: Pattern -> Int -> [Pattern]
updateToList pattern 0            = []
updateToList pattern generations  = pattern :
                                    updateToList (updatePattern pattern)
                                    (generations - 1)

{-
  This is a test that checks whether the core functions work correctly.
  It comes handy when these are modified.
-}

test :: IO ()
test = do
        patternString <- readFile "gol_patterns/pulsar.grid"
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
        if  showGridSimple pattern1 "" == patternString1
          then putStrLn "- showGridSimple       OK"
          else putStrLn "- showGridSimple       not OK"
        if  showGridFancy pattern1 (== Alive) == fSpattern1
          then putStrLn "- showGridFancy        OK"
          else putStrLn "- showGridFancy        not OK"

patternString1  = ".....\n..#..\n.###.\n.###.\n..#..\n....."
pattern1        = readPattern patternString1
fSpattern1      = "\9487\9473\9473\9473\9491\n\9475 \9622 \9475\
                  \\n\9475\9616\9608 \9475\n\9475 \9624 \9475\
                  \\n\9495\9473\9473\9473\9499"
patternString2  = ".....\n.###.\n.....\n.....\n.###.\n....."
pattern2        = readPattern patternString2
