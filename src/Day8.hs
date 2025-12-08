module Day8 (main) where

import Misc (check)
import Par4 (parse,Par,separated,int,lit,nl)
import Data.Set (Set,member)
import Data.Set qualified as Set
import Data.List qualified as List
import Data.Ord (comparing)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day8.sample"
  inp <- parse gram <$> readFile "input/day8.input"
  print ("day8, part1", check 40 $ part1 10 sam)
  print ("day8, part1", check 42315 $ part1 1000 inp)
  print ("day8, part2 (sample)", check 25272 $ part2 sam)
  print ("day8, part2", check 8079278220 $ part2 inp)

part1 :: Int -> [Point] -> Int
part1 n = do
  product . take 3 . reverse . List.sort
    . sizes . foldl connect empty
    . take n . List.sortBy (comparing distance) . pairings

part2 :: [Point] -> Int
part2 ps = do
  let xs = List.sortBy (comparing distance) (pairings ps)
  loop empty xs
    where
      target = [length ps]
      loop :: State -> [(Point,Point)] -> Int
      loop acc = \case
        [] -> undefined
        pq:pqs -> do
          let acc' = connect acc pq
          if sizes acc' /= target then loop acc' pqs else do
            let ((a,_,_),(b,_,_)) = pq
            (a*b)

pairings :: [a] -> [(a,a)]
pairings = \case [] -> []; x:xs -> [ (x,y) | y <- xs ] ++ pairings xs

data State = X [ Set Point ] deriving Show

sizes :: State -> [Int]
sizes (X sets) = [ Set.size set | set <- sets ]

empty :: State
empty = X []

connect :: State -> (Point,Point) -> State
connect (X sets) (p,q) = do
  let pset = find p
  let qset = find q
  let pqset = pset `Set.union` qset
  X (pqset : [ set | set <- sets, not (p `member` set) && not (q `member` set) ])
  where
    find :: Point -> Set Point
    find x = case [ set | set <- sets, x `member` set ] of
      [] -> Set.singleton x
      [set] -> set
      _ -> error "find"

type Point = (Int,Int,Int)

distance :: (Point,Point) -> Int
distance ((x1,y1,z1),(x2,y2,z2)) = sqr (x1-x2) + sqr (y1-y2) + sqr (z1-z2)
  where sqr n = n*n

gram :: Par [Point]
gram = separated nl point
  where point = do x <- int; lit ','; y <- int; lit ','; z <- int; pure (x,y,z)
