module Day10 (main) where

import Misc (check,hist)
import Par4 (parse,Par,many,terminated,separated,alts,int,lit,nl)
import Data.Map qualified as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day10.sample"
  part1_sam <- part1 sam
  print ("day10, part1 (sample-details)", check [2,3,2] $ part1_sam)
  print ("day10, part1 (sample)", check 7 $ sum $ part1_sam)
  inp <- parse gram <$> readFile "input/day10.input"
  part1_inp <- part1 inp
  print ("day10, part1", check 461 $ sum $ part1_inp)

part1 :: [Machine] -> IO [Int]
part1 = mapM solve1

solve1 :: Machine -> IO Int
solve1 Machine{lights,buttons} = do
  let goal = [ n | (n,b) <- zip [0::Int ..] lights, b]
  --print goal
  let cands = power (length buttons)
  let
    testCand bs = do
      let bbs = zip buttons bs
      let collapse = [ x | (Button xs,bool) <- bbs, bool, x<- xs ]
      let xns = Map.toList (hist collapse)
      let odds = [ x | (x,n) <- xns, n `mod` 2 == 1 ]
      let match = (odds == goal)
      let num = length [ () | b <- bs, b ]
      (bs, num, collapse, odds, match)

  let trials = [ testCand cand | cand <- cands ]
  --mapM_ print trials
  pure $ minimum [ num | (_,num,_,_,match) <- trials, match ]

power :: Int -> [[Bool]]
power n =
  if n < 0 then undefined else
    if n == 0 then [[]] else do
      let bss = power (n-1)
      [ False:bs | bs <- bss ] ++ [ True:bs | bs <- bss ]

data Machine = Machine
  { lights :: [Bool]
  , buttons :: [Button]
  , joltage :: [Int]
  } deriving Show

data Button = Button [Int] deriving Show

gram :: Par [Machine]
gram = separated nl machine
  where
    machine = do
      lit '['
      lights <- many light
      lit ']'
      lit ' '
      buttons <- terminated (lit ' ') button
      lit '{'
      joltage <- ints
      lit '}'
      pure Machine { lights, buttons, joltage }

    light = alts [ do lit '.'; pure False
                 , do lit '#'; pure True ]

    button = Button <$> do
      lit '('
      xs <- ints
      lit ')'
      pure xs

    ints = separated (lit ',') int
