module Advent2025 (main) where

import System.Environment (getArgs)
--import Timing (timed,printTimings)

import qualified Day1
import qualified Day2
import qualified Day3

mains :: [(Int,IO ())]
mains = zip [1..]
  [ Day1.main
  , Day2.main
  , Day3.main
  ]

main :: IO ()
main = do
  args <- getArgs
  let selected = if args == [] then map fst mains else map read args
  let picked = [ x | x@(i,_) <- mains, i `elem` selected ]
  --_info <- sequence [ timed day io | (day,io) <- picked ]
  --if length picked > 1 then printTimings _info else pure ()
  sequence_ [ io | (_,io) <- picked ]
