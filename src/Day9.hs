module Day9 (main) where

import Misc (check)
import Par4 (parse,Par,separated,int,lit,nl)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day9.sample"
  inp <- parse gram <$> readFile "input/day9.input"
  print ("day9, part1 (sample)", check 50 $ part1 sam)
  print ("day9, part1", check 4759531084 $ part1 inp)

part1 :: Puz -> Int
part1 ps = maximum [ area (a,b) | (a,b) <- pairings ps ]

pairings :: [a] -> [(a,a)]
pairings = \case [] -> []; x:xs -> [ (x,y) | y <- xs ] ++ pairings xs

type Puz = [Point]
type Point = (Int,Int)

area :: (Point,Point) -> Int
area ((x1,y1),(x2,y2)) = (1 + abs (x1-x2)) * (1 + abs (y1-y2))

gram :: Par [Point]
gram = separated nl point
  where point = do x <- int; lit ','; y <- int; pure (x,y)
