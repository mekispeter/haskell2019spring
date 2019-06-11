{-
  Functional Programming for Logicians 2019 Spring

  This module introduces the Grid type, which is basically
  a two-dimensional vector, used in some of the projects.
  It is an instance of the Foldable and Monad classes.
  It has various show methods, including some that write
  image files.
-}

module Grid where

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.List as L
import Codec.Picture

{-
  Definition and a few basic functions
-}

newtype Grid a = Grid (V.Vector (V.Vector a)) deriving (Eq, Show)

(!!!) :: Grid a -> (Int, Int) -> a
(Grid xss) !!! (x, y)           = (xss ! y) ! x

gridSizeY :: Grid a -> Int
gridSizeY (Grid xss)            = V.length xss

gridSizeX :: Grid a -> Int
gridSizeX (Grid xss)
  | gridSizeY (Grid xss) == 0   = 0
  | otherwise                   = V.length $ V.head xss

gridLength :: Grid a -> Int
gridLength grid                 = gridSizeX grid * gridSizeY grid

isEmpty :: Grid a -> Bool
isEmpty grid                    = gridLength grid == 0

vectorFromGrid :: Grid a -> V.Vector (V.Vector a)
vectorFromGrid (Grid xss)       = xss

gridFromList :: [[a]] -> Grid a
gridFromList xss                = Grid $ V.fromList (map V.fromList xss)

listFromGrid :: Grid a -> [[a]]
listFromGrid (Grid xss)         = map V.toList (V.toList xss)

listFromOuter :: Grid a -> [V.Vector a]
listFromOuter (Grid xss)        = V.toList xss

gridConcat :: Grid (Grid a) -> Grid a
gridConcat grid                 = gridFromList $
                                  concat $
                                  map ((map concat) . L.transpose)
                                  ((map . map) listFromGrid $
                                  listFromGrid grid)
-- g v v g v v  grid
--   l l g v v  listFromGrid
--   l l   l l  (map . map) listFromGrid
--   l l     l  map ((map concat) . L.transpose)
--     l     l  concat
-- g   v     v  gridFromList

{-
  Instantiations
-}

instance Functor Grid where
  fmap f (Grid xss)             = Grid $ (fmap . fmap) f xss

instance Applicative Grid where
  pure x                        = gridFromList [[x]]
  fgrid <*> grid                = gridConcat $ fmap (\f -> fmap f grid) fgrid

instance Monad Grid where
  return x                      = pure x
  grid >>= f                    = gridConcat $ fmap f grid

instance Foldable Grid where
  foldr f x grid                = foldr f x (concat $ listFromGrid grid)


--------------------------------------------------------
-- Applicative:
-- ┏━━━━━━━━━━━━━┓     ┏━━━━━━━━━━━━━┓
-- ┃ f00 f10 ... ┃     ┃ a00 a10 ... ┃
-- ┃ f01 f11 ... ┃ <*> ┃ a01 a11 ... ┃  =
-- ┃ ... ... ... ┃     ┃ ... ... ... ┃
-- ┗━━━━━━━━━━━━━┛     ┗━━━━━━━━━━━━━┛
-- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
-- ┃ (f00 a00) (f00 a10) ... (f10 a00) (f10 a10) ... ┃
-- ┃ (f00 a01) (f00 a11) ... (f10 a01) (f10 a11) ... ┃
-- ┃    ...       ...    ...    ...       ...    ... ┃
-- ┃ (f01 a00) (f01 a10) ... (f11 a00) (f11 a10) ... ┃
-- ┃    ...       ...    ...    ...       ...    ... ┃
-- ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
----------------------------------------------------------
-- Monad:
--         ┏━━━━━━━━━━━━━┓         ┏━━━━━━━━━━━━━┓
--         ┃ b00 b10 ... ┃         ┃ c00 c10 ... ┃
-- f a00 = ┃ b01 b11 ... ┃ f a10 = ┃ c01 c11 ... ┃ ...
--         ┃ ... ... ... ┃         ┃ ... ... ... ┃
--         ┗━━━━━━━━━━━━━┛         ┗━━━━━━━━━━━━━┛
--         ┏━━━━━━━━━━━━━┓         ┏━━━━━━━━━━━━━┓
--         ┃ k00 k10 ... ┃         ┃ l00 l10 ... ┃
-- f a01 = ┃ k01 k11 ... ┃ f a11 = ┃ l01 l11 ... ┃ ...
--         ┃ ... ... ... ┃         ┃ ... ... ... ┃
--         ┗━━━━━━━━━━━━━┛         ┗━━━━━━━━━━━━━┛
--               ...                     ...       ...
--                           ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓
-- ┏━━━━━━━━━━━━━┓           ┃ b00 c00 ... b10 c10 ... ┃
-- ┃ a00 a10 ... ┃           ┃ k00 l00 ... k10 l10 ... ┃
-- ┃ a01 a11 ... ┃ >>= f  =  ┃ ... ... ... ... ... ... ┃
-- ┃ ... ... ... ┃           ┃ b01 c01 ... b11 c11 ... ┃
-- ┗━━━━━━━━━━━━━┛           ┃ k01 l01 ... k11 l11 ... ┃
--                           ┃ ... ... ... ... ... ... ┃
--                           ┗━━━━━━━━━━━━━━━━━━━━━━━━━┛
----------------------------------------------------------
-- Concatenation:
-- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
-- ┃┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓┃
-- ┃┃┏━━━━━━━━━┓ ┏━━━━━━━━━┓ ┏━━━━━━━━━┓ ┏━━━━━━━━━┓┃┃
-- ┃┃┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃┃┃
-- ┃┃┃┃ # # # ┃┃ ┃┃ @ @ @ ┃┃ ┃┃ > > > ┃┃ ┃┃ * * * ┃┃┃┃
-- ┃┃┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃┃┃
-- ┃┃┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃┃┃
-- ┃┃┃┃ - - - ┃┃ ┃┃ . . . ┃┃ ┃┃ < < < ┃┃ ┃┃ ; ; ; ┃┃┃┃
-- ┃┃┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃┃┃
-- ┃┃┗━━━━━━━━━┛ ┗━━━━━━━━━┛ ┗━━━━━━━━━┛ ┗━━━━━━━━━┛┃┃
-- ┃┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛┃
-- ┃┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓┃
-- ┃┃┏━━━━━━━━━┓ ┏━━━━━━━━━┓ ┏━━━━━━━━━┓ ┏━━━━━━━━━┓┃┃
-- ┃┃┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃┃┃
-- ┃┃┃┃ + + + ┃┃ ┃┃ ^ ^ ^ ┃┃ ┃┃ " " " ┃┃ ┃┃ / / / ┃┃┃┃
-- ┃┃┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃┃┃
-- ┃┃┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃ ┃┏━━━━━━━┓┃┃┃
-- ┃┃┃┃ ! ! ! ┃┃ ┃┃ ? ? ? ┃┃ ┃┃ \ \ \ ┃┃ ┃┃ % % % ┃┃┃┃
-- ┃┃┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃ ┃┗━━━━━━━┛┃┃┃
-- ┃┃┗━━━━━━━━━┛ ┗━━━━━━━━━┛ ┗━━━━━━━━━┛ ┗━━━━━━━━━┛┃┃
-- ┃┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛┃
-- ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
-- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
-- ┃┏━━━━━━━━━━━━━━━━━━━━━━━━━┓┃
-- ┃┃ # # # @ @ @ > > > * * * ┃┃
-- ┃┗━━━━━━━━━━━━━━━━━━━━━━━━━┛┃
-- ┃┏━━━━━━━━━━━━━━━━━━━━━━━━━┓┃
-- ┃┃ - - - . . . < < < ; ; ; ┃┃
-- ┃┗━━━━━━━━━━━━━━━━━━━━━━━━━┛┃
-- ┃┏━━━━━━━━━━━━━━━━━━━━━━━━━┓┃
-- ┃┃ + + + ^ ^ ^ " " " / / / ┃┃
-- ┃┗━━━━━━━━━━━━━━━━━━━━━━━━━┛┃
-- ┃┏━━━━━━━━━━━━━━━━━━━━━━━━━┓┃
-- ┃┃ ! ! ! ? ? ? \ \ \ % % % ┃┃
-- ┃┗━━━━━━━━━━━━━━━━━━━━━━━━━┛┃
-- ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

{-
  Show functions
-}

showGridSimple :: Show a => Grid a -> String -> String
showGridSimple grid delimiter
  | isEmpty grid                = ""
  | otherwise                   = concatWith "\n" $
                                  map (concatWith delimiter)
                                  ((map . map) show $ listFromGrid grid)

showGridAlign :: Show a => Grid a -> String -> String
showGridAlign grid delimiter
  | isEmpty grid                = ""
  | otherwise                   = concatWith "\n" $
                                  map (concatWith delimiter)
                                  ((map . map) (extend maxLength) showCells)
                                  where
                                    showCells = listFromGrid $ fmap show grid
                                    maxLength = maximum
                                                [maximum [length x | x <- row]
                                                | row <- showCells]

concatWith :: String -> [String] -> String
concatWith delimiter xss        = foldr1 (\x y -> x ++ delimiter ++ y) xss

extend :: Int -> String -> String
extend n xs
  | n <= length xs  = xs
  | otherwise       = xs ++ replicate (n - length xs) ' '

showGridFancy :: (Show a, Eq a) => Grid a -> (a -> Bool) -> String
showGridFancy grid property = concatWith "\n"
                           (fancyShowAddBorder $
                           fancyShowAsList grid property)

fancyShowAsList :: (Show a, Eq a) => Grid a -> (a -> Bool) -> [String]
fancyShowAsList grid property
                          = [
                              [fancyQuarter (getQuarter grid x y) property
                                | x <- [0..maxX]]
                              | y <- [0..maxY]
                            ]
                            where
                              maxX = (gridSizeX grid - 1) `div` 2
                              maxY = (gridSizeY grid - 1) `div` 2

fancyChars :: String
fancyChars =   " ▗▖▄▝▐▞▟▘▚▌▙▀▜▛█"

fancyQuarter :: Eq a => (Maybe a, Maybe a, Maybe a, Maybe a) ->
                (a -> Bool) -> Char
fancyQuarter (ul, ur, bl, br) property
                          = fancyChars !!
                          (8*(i ul) + 4*(i ur) + 2*(i bl) + i br)
                          where
                            i Nothing   = 0
                            i (Just x)  = if property x
                                            then 1
                                            else 0

getQuarter :: Grid a -> Int -> Int ->
              (Maybe a, Maybe a, Maybe a, Maybe a)
getQuarter grid x y
              = (
                  Just $ grid !!! (2*x, 2*y),
                  if gridSizeX grid <= 2*x+1
                    then Nothing
                    else Just $ grid !!! (2*x+1, 2*y),
                  if gridSizeY grid <= 2*y+1
                    then Nothing
                    else Just $ grid !!! (2*x, 2*y+1),
                  if gridSizeX grid <= 2*x+1 || gridSizeY grid <= 2*y+1
                    then Nothing
                    else Just $ grid !!! (2*x+1, 2*y+1)
                )

fancyShowAddBorder :: [String] -> [String]
fancyShowAddBorder xss = [topLine] ++ map lineEndings xss ++ [bottomLine] where
  topLine = "┏" ++ replicate (length $ head xss) '━' ++ "┓"
  lineEndings xs = "┃" ++ xs ++ "┃"
  bottomLine = "┗" ++ replicate (length $ head xss) '━' ++ "┛"

{-
  Writing grids to image files
-}

writeGridToPNG :: Grid a -> String -> (a -> PixelRGB8) -> Int
                  -> IO()
writeGridToPNG grid name renderPixel zoom
              = writePng (name ++ ".png") $
                generateImage getPixels sizeX sizeY
                where
                  sizeX = gridSizeX grid * zoom
                  sizeY = gridSizeY grid * zoom
                  getPixels x y = renderPixel $ grid !!!
                    (x `div` zoom, y `div` zoom)

writeGridsToGif :: [Grid a] -> String -> (a -> PixelRGB8) -> Int
                   -> IO()
writeGridsToGif gridList name renderPixel zoom
              = getright $
                writeGifAnimation (name ++ ".gif")
                25 LoopingNever $
                [generateImage (getPixels grid) sizeX sizeY | grid <- gridList]
                where
                  getright (Right x)  = x
                  sizeX               = zoom * (gridSizeX $ head gridList)
                  sizeY               = zoom * (gridSizeY $ head gridList)
                  getPixels grid x y  = renderPixel $ grid !!!
                                        (x `div` zoom, y `div` zoom)
