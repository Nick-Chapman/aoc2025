module Day11 (main) where

import Data.Map (Map)
import Misc (check,look)
import Par4 (parse,Par,separated,key,word,sp,nl)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day11.sample"
  inp <- parse gram <$> readFile "input/day11.input"
  sam2 <- parse gram <$> readFile "input/day11.sample2"
  print ("day11, part1 (sample)", check 5 $ part1 sam)
  print ("day11, part1", check 714 $ part1 inp)
  print ("day11, part2 (sample2)", check 2 $ part2 sam2)
  print ("day11, part2", check 333852915427200 $ part2 inp)

part1 :: Graph -> Int
part1 g = length $ pathFrom "you"
  where
    edges = Map.fromList g
    pathFrom :: String -> [[String]]
    pathFrom x = do
      if x == "out" then [["out"]] else do
        concat [ [ x:path | path <- pathFrom y ] | y <- look x edges ]

part2 :: Graph -> Int
part2 g0 =
  (look "svr" to_fft * look "fft" to_dac * look "dac" to_out)
  + (look "svr" to_dac * look "dac" to_fft * look "fft" to_out)
  where
    g = ("out",[]) : g0
    edges = Map.fromList g

    to_dac = to_dest "dac"
    to_fft = to_dest "fft"
    to_out = to_dest "out"

    to_dest :: String -> Map String Int
    to_dest dest = m
      where
        m = Map.fromList [ (x,f x) | (x,_) <- g ]
        f x = if x == dest then 1 else sum [ look y m | y <- look x edges ]

type Graph = [ForwardLink]
type ForwardLink = (String,[String])

gram :: Par Graph
gram = separated nl line
  where
    line :: Par ForwardLink
    line = do x <- word; key ": "; ys <- separated sp word; pure (x,ys)
