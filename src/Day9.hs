module Day9 (main) where

import Prelude hiding (head,tail)
import Misc (check,head,tail)
import Par4 (parse,Par,separated,int,lit,nl)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day9.sample"
  inp <- parse gram <$> readFile "input/day9.input"
  print ("day9, part1 (sample)", check 50 $ part1 sam)
  print ("day9, part1", check 4759531084 $ part1 inp)
  print ("day9, part2 (sample)", check 24 $ part2 sam)
  print ("day9, part2", check 1539238860 $ part2 inp)

part1 :: Puz -> Int
part1 ps = maximum [ area a b | (a,b) <- pairings ps ]

part2 :: Puz -> Int
part2 ps = do
  let edges = [ edgeDir p q | (p,q) <- zip ps (tail ps ++ [head ps]) ]
  maximum [ area a b  | (a,b) <- pairings ps , not (any (crosses a b) edges) ]

data Edge = H (Int,Int) Int | V Int (Int,Int)

edgeDir :: Point -> Point -> Edge
edgeDir (x1,y1) (x2,y2) =
  if x1==x2 then V x1 (y1,y2) else
    if y1==y2 then H (x1,x2) y1 else
      error "edgeDir"

crosses :: Point -> Point -> Edge -> Bool
crosses (ax,ay) (bx,by) edge = do
  let (xL,xH) = order ax bx
  let (yL,yH) = order ay by
  case edge of
    H (x1,x2) y -> (yL < y && y < yH) && ((x1 > xL) || (x2 > xL)) && ((x1 < xH) || (x2 < xH))
    V x (y1,y2) -> (xL < x && x < xH) && ((y1 > yL) || (y2 > yL)) && ((y1 < yH) || (y2 < yH))
  where
    order :: Int -> Int -> (Int,Int)
    order a b = if a < b then (a,b) else (b,a)

pairings :: [a] -> [(a,a)]
pairings = \case [] -> []; x:xs -> [ (x,y) | y <- xs ] ++ pairings xs

type Puz = [Point]
type Point = (Int,Int)

area :: Point -> Point -> Int
area (x1,y1) (x2,y2) = (1 + abs (x1-x2)) * (1 + abs (y1-y2))

gram :: Par [Point]
gram = separated nl point
  where point = do x <- int; lit ','; y <- int; pure (x,y)
