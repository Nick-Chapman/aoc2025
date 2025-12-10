module Advent2025 (main) where

import System.Environment (getArgs)
import Timing (timed,printTimings)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

mains :: [(Int,IO ())]
mains = zip [1..]
  [ Day1.main
  , Day2.main
  , Day3.main
  , Day4.main
  , Day5.main
  , Day6.main
  , Day7.main
  , Day8.main
  , Day9.main
  ]

main :: IO ()
main = do
  args <- getArgs
  let selected = if args == [] then map fst mains else map read args
  let picked = [ x | x@(i,_) <- mains, i `elem` selected ]
  _info <- sequence [ timed day io | (day,io) <- picked ]
  if length picked > 1 then printTimings _info else pure ()
