module Day6 (main) where

import Misc (check)
import Par4 (parse,Par,separated,dot,many,nl)
import Data.List (transpose)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day6.sample"
  inp <- parse gram <$> readFile "input/day6.input"
  print ("day6, part1 (sample)", check 4277556 $ part1 sam)
  print ("day6, part1", check 6417439773370 $ part1 inp)
  print ("day6, part2 (sample)", check 3263827 $ part2 sam)
  print ("day6, part2", check 11044319475191 $ part2 inp)
  where
    part1 = solve Part1
    part2 = solve Part2

data Part = Part1 | Part2

solve :: Part -> [String] -> Int
solve part lines = do
  let opLine = last lines
  let rest = init lines
  let stops = [ p | (p,c) <- zip [0::Int ..] opLine, c /= ' ' ]
  let ws = [ b - a | (a,b) <- zip stops (drop 1 stops) ]
  let xss = transpose [ splitLine line ws | line <- rest ]
  let f = case part of Part1 -> id; Part2 -> map transpose
  let ops = [ case c of '*' -> Mul; '+' -> Add; _ -> error "op" | c <- opLine, c /= ' ' ]
  sum [ apply op (map read xs) | (xs,op) <- zip (f xss) ops ]

splitLine :: String -> [Int] -> [String]
splitLine str = \case
  [] -> [str]
  w:ws -> do
    let (front,back) = splitAt (w-1) str
    front : splitLine (drop 1 back) ws

data Op = Add | Mul deriving Show

apply :: Op -> [Int] -> Int
apply = \case Add -> foldl (+) 0; Mul -> foldl (*) 1

gram :: Par [String]
gram = separated nl (many dot)
