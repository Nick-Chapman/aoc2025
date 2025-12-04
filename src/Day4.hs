module Day4 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,many,alts,lit)
import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day4.sample"
  inp <- parse gram <$> readFile "input/day4.input"
  print ("day4, part1 (sample)", check 13 $ length $ part1 sam)
  print ("day4, part1", check 1395 $ length $ part1 inp)
  print ("day4, part2 (sample)", check 43 $ sum $ part2 sam)
  print ("day4, part2", check 8451 $ sum $ part2 inp)

part1 :: Grid -> [Pos]
part1 Grid{size,m} = do
  let allPos = [ (a,b) | a <- [0..size-1], b <- [0..size-1] ]
  let isBlocked pos = (maybe Space id $ Map.lookup pos m) == Paper
  [ x | x <- allPos, isBlocked x, let n = length [ () | y <- adjacent x, isBlocked y ], n < 4 ]
  where
    adjacent :: Pos -> [Pos]
    adjacent (x,y) = [ (x',y') | x' <- [x-1,x,x+1] , y' <- [y-1,y,y+1] , not (x'==x && y'==y) ]

part2 :: Grid -> [Int]
part2 = loop where
  loop grid@Grid{m} = do
    let ps = part1 grid
    let a = length ps
    if a == 0 then [] else a : loop grid { m = foldr Map.delete m ps }

data Grid = Grid { size :: Int, m :: Map Pos Cell }
type Pos = (Int,Int)
data Cell = Paper | Space deriving Eq

mkGrid :: [[Cell]] -> Grid
mkGrid bss = Grid
  { size = length bss
  , m = Map.fromList[ ((x,y),b) | (y,bs) <- zip [0..] bss, (x,b) <- zip [0..] bs ]
  }

gram :: Par Grid
gram = mkGrid <$> separated nl line where
  line = many cell
  cell = alts [ do lit '.'; pure Space
              , do lit '@'; pure Paper ]
